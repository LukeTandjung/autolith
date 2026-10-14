(in-package #:autolith)


(defparameter *sidebar-trace-page-records* 128)


(defparameter *sidebar-view-cache-limit*
  4
  "Maximum cached trace viewports, including the selected view; PRIMARY is separate.")

(-> sidebar-cache-view (sidebar-terminal-ui string list) list)


(defun sidebar-cache-view (ui identifier view)
  "Retain the selected viewport and a small least-recently-selected cache."
  (setf (gethash identifier (sidebar-views ui)) view
        (sidebar-view-order ui)
          (cons identifier (remove identifier (sidebar-view-order ui) :test #'equal)))
  (let* ((limit (max 1 *sidebar-view-cache-limit*)) (order (sidebar-view-order ui)))
    (dolist (expired (nthcdr (min limit (length order)) order))
      (remhash expired (sidebar-views ui)))
    (setf (sidebar-view-order ui) (subseq order 0 (min limit (length order)))))
  view)

(-> sidebar-owned-trace-segments (pathname (option pathname)) list)


(defun sidebar-owned-trace-segments (identity directory)
  "Apply native task-inspector ownership checks to every resolved chunk."
  (let ((segments (conversation-storage-pathnames identity)))
    (unless
        (and segments
             (or (null directory)
                 (every
                  (lambda (path)
                    (uiop/pathname:subpathp (platform-truename *platform* path)
                                            (platform-truename *platform* directory)))
                  segments)))
      (task-continuity--fail "No owned transcript artifact is available."))
    segments))

(-> sidebar-revoke (sidebar-terminal-ui (option string)) t)


(defun sidebar-revoke (ui owner)
  "Revoke cached trace capabilities when the primary ownership root changes."
  (incf (sidebar-epoch ui))
  (setf (sidebar-ui-owner ui) owner
        (sidebar-selected ui) nil
        (sidebar-hits ui) nil
        (sidebar-offset ui) 0
        (sidebar-view-error ui) nil)
  (setf (sidebar-view-order ui) nil)
  (clrhash (sidebar-views ui))
  (clrhash (sidebar-linked-rows ui))
  (handler-case (sidebar-restore-trace-links ui owner)
                (error (condition)
                       (setf (sidebar-view-error ui) (princ-to-string condition)))))

(-> sidebar-current-view (sidebar-terminal-ui) (option list))


(defun sidebar-current-view (ui)
  "Return the session rail's sidebar-current-view."
  (and (sidebar-selected ui) (gethash (sidebar-selected ui) (sidebar-views ui))))

(-> sidebar-resolve-trace (application sidebar-terminal-ui string) list)


(defun sidebar-resolve-trace (application ui identifier)
  "Resolve an owned child or inference identity, never arbitrary file paths."
  (let* ((viewer (application-agent application))
         (owner (and viewer (task-parent-root-conversation-identifier viewer)))
         (orchestrator (application--task-orchestrator application)))
    (unless (and owner (equal owner (sidebar-ui-owner ui)))
      (sidebar--fail "The primary session changed during trace selection."))
    (let ((frame
           (find identifier (sidebar-inventory ui) :key (lambda (row) (getf row :id))
                 :test #'equal)))
      (if (and frame (member (getf frame :kind) '(:root :rlm)))
          (let ((trace (getf frame :trace)))
            (unless (and trace (rlm--trace-identifier-p trace))
              (sidebar--fail "This inference has not published a trace yet."))
            (list :path
                  (merge-pathnames (make-pathname :name trace :type "sexp")
                                   (configuration-inference-root
                                    (application-configuration application)))
                  :trace trace :owner owner :name identifier :directory
                  (configuration-inference-root
                   (application-configuration application))))
          (let* ((entry (task-continuity--find viewer identifier))
                 (record (getf entry :record))
                 (directory
                  (uiop/pathname:pathname-directory-pathname (getf entry :path)))
                 (live
                  (and orchestrator
                       (find (getf record :execution-id)
                             (task-orchestrator-list-visible-jobs orchestrator viewer)
                             :key #'session-job-execution-identifier :test #'equal)))
                 (result-path (merge-pathnames "result.sexp" directory))
                 (result
                  (or (and live (getf (session-job-snapshot live) :result))
                      (and (probe-file result-path)
                           (task--read-result-artifact result-path))))
                 (path
                  (or (getf result :conversation-file)
                      (merge-pathnames
                       (make-pathname :name (getf record :execution-id) :type "sexp")
                       directory))))
            (unless
                (and (eq (getf record :kind) :task)
                     (or (null live) (typep live 'task-job)))
              (sidebar--fail "This is a tool job, not a conversation session."))
            (sidebar-owned-trace-segments (pathname path) directory)
            (list :path (pathname path) :directory directory :owner owner :name
                  identifier))))))

(-> sidebar-read-page (pathname integer &optional boolean (option pathname))
    list)


(defun sidebar-read-page (pathname start &optional tail-p owned-directory)
  "Read one bounded raw-record page through native validated segment mapping.

Skip earlier deterministic chunks for forward pages. Tail scans newest chunks
only, retaining a fixed ring of records. There is no preview horizon and no
conversation reconstruction. Incomplete writer tails are retried on refresh."
  (let* ((identity (conversation-storage-identity-pathname pathname))
         (segments (sidebar-owned-trace-segments identity owned-directory))
         (records nil)
         (count 0)
         (more-p nil))
    (unless segments (sidebar--fail "The durable session trace is missing."))
    (if tail-p
        (dolist (segment (reverse segments))
          (when (= count *sidebar-trace-page-records*) (return))
          (let* ((wanted (- *sidebar-trace-page-records* count))
                 (ring (make-array wanted :initial-element nil))
                 (seen 0))
            (conversation--map-segment-records identity segment
                                               (lambda (record)
                                                 (when (getf (rest record) :seq)
                                                   (setf (aref ring (mod seen wanted))
                                                           record)
                                                   (incf seen))))
            (let* ((kept (min seen wanted))
                   (offset
                    (if (> seen wanted)
                        (mod seen wanted)
                        0))
                   (batch
                    (loop for index below kept
                          collect (aref ring (mod (+ offset index) wanted)))))
              (incf count kept)
              (setf records (append batch records)))))
        (let ((first-index
               (or
                (position-if
                 (lambda (segment)
                   (<= (conversation-search--segment-start segment) start))
                 segments :from-end t)
                0)))
          (block page
            (dolist (segment (nthcdr first-index segments))
              (conversation--map-segment-records identity segment
                                                 (lambda (record)
                                                   (let ((sequence
                                                          (getf (rest record) :seq)))
                                                     (when
                                                         (and sequence
                                                              (>= sequence start))
                                                       (when
                                                           (= count
                                                              *sidebar-trace-page-records*)
                                                         (setf more-p t)
                                                         (return-from page nil))
                                                       (push record records)
                                                       (incf count)))))))
          (setf records (nreverse records))))
    (list :records records :start
          (or (and records (getf (rest (first records)) :seq)) start) :next
          (if records
              (1+ (getf (rest (car (last records))) :seq))
              start)
          :more-p more-p)))

(-> sidebar-renderer (sidebar-terminal-ui fullscreen-terminal-ui) application)


(defun sidebar-renderer (ui trace-ui)
  "Render selected sessions with an isolated copy of main presentation settings."
  (let* ((application (sidebar-application ui))
         (renderer (allocate-instance (find-class 'application)))
         (primary-configuration
          (and application (slot-boundp application 'configuration)
               (application-configuration application)))
         (configuration
          (configuration-create :durable-p nil :defer-provider-validation-p t
                                :compact-view-p
                                (if primary-configuration
                                    (config :compact-view-p primary-configuration)
                                    t)
                                :reasoning-traces-p
                                (and primary-configuration
                                     (config :reasoning-traces-p
                                             primary-configuration)))))
    (setf (slot-value renderer 'configuration) configuration
          (slot-value renderer 'ui) trace-ui
          (slot-value renderer 'tool-registry)
            (if (and application (slot-boundp application 'tool-registry))
                (application-tool-registry application)
                (make-instance 'tool-registry)))
    renderer))

(-> sidebar-trace-entry (application list) t)


(defun sidebar-trace-entry (renderer record)
  "Present a selected session through normal conversation entries, not raw records."
  (or (conversation-record-entry renderer record)
      (when (eq (first record) :message)
        (let* ((properties (rest record))
               (role (getf properties :role))
               (content (getf properties :content)))
          (when (and (eq role :assistant) (stringp content))
            (append
             (application--transcript-entry renderer :style ':brand :header "● autolith"
                                            :timestamp (getf properties :time))
             (list (terminal-span ':plain (string #\Newline)))
             (application--markdown-body renderer content)))))))

(-> sidebar-load-page
    (sidebar-terminal-ui list integer &key (:tail-p boolean)
     (:preserve-scroll-p boolean))
    t)


(defun sidebar-load-page (ui view start &key tail-p preserve-scroll-p)
  "Replace only VIEW's bounded viewport, committing a complete page after successful rendering."
  (let* ((signature (sidebar-trace-signature (getf view :path) (getf view :directory)))
         (page (sidebar-read-page (getf view :path) start tail-p (getf view :directory)))
         (trace-ui (getf view :ui))
         (old (fullscreen-terminal-ui-viewport trace-ui))
         (viewport (clinedi:make-transcript-viewport)))
    (unwind-protect
        (progn
         (setf (slot-value trace-ui 'viewport) viewport)
         (let* ((renderer (sidebar-renderer ui trace-ui))
                (*sidebar-sizing-terminal* (terminal-ui-terminal ui))
                (*sidebar-sizing-width*
                 (or (sidebar-main-width ui)
                     (let ((*sidebar-sizing-terminal* nil))
                       (nth-value 0
                                  (sidebar-layout-widths
                                   (terminal-columns (terminal-ui-terminal ui))))))))
           (terminal-ui-fullscreen--ensure-width trace-ui *sidebar-sizing-width*)
           (dolist (record (getf page :records))
             (let ((entry (sidebar-trace-entry renderer record)))
               (when entry
                 (multiple-value-bind (plain display regions)
                     (terminal-ui--finalized-content trace-ui entry)
                   (terminal-ui-fullscreen--append trace-ui plain display regions))))))
         (if tail-p
             (clinedi:transcript-viewport-follow viewport)
             (clinedi:transcript-viewport-scroll-to-top viewport))
         (when preserve-scroll-p
           (clinedi:transcript-viewport-scroll viewport (getf view :top 0)))
         (setf (getf view :start) (getf page :start)
               (getf view :next) (getf page :next)
               (getf view :more-p) (getf page :more-p)
               (getf view :tail-p) tail-p
               (getf view :signature) signature
               (getf view :error) nil)
         (setf old nil))
      (when old (setf (slot-value trace-ui 'viewport) old))))
  view)

(-> sidebar-select (sidebar-terminal-ui (or string (member nil :primary)))
    boolean)


(defun sidebar-select (ui identifier)
  "Select immediately; PRIMARY restores its untouched live editor and transcript."
  (when (or (null identifier) (eq identifier :primary))
    (setf (sidebar-selected ui) nil
          (sidebar-view-error ui) nil)
    (terminal-ui-fullscreen-invalidate ui)
    (return-from sidebar-select t))
  (sidebar-inventory ui)
  (let ((epoch (sidebar-epoch ui)) (owner (sidebar-ui-owner ui)))
    (handler-case
     (let* ((capability
             (and (sidebar-transcript-function ui)
                  (funcall (sidebar-transcript-function ui) identifier)))
            (view (gethash identifier (sidebar-views ui))))
       (unless capability (sidebar--fail "No durable session trace is available."))
       (when
           (and view
                (some (lambda (key) (not (equal (getf view key) (getf capability key))))
                      '(:path :directory :owner :trace)))
         (setf view nil))
       (unless view
         (setf view
                 (append capability
                         (list :ui
                               (terminal-ui-create :terminal (terminal-ui-terminal ui)
                                                   :fullscreen-p t)
                               :refreshed 0 :top 0 :start 1 :next 1 :more-p nil :tail-p t
                               :error nil)))
         (sidebar-load-page ui view 1 :tail-p t))
       (sidebar-inventory ui)
       (when (and (= epoch (sidebar-epoch ui)) (equal owner (sidebar-ui-owner ui)))
         (setf (sidebar-selected ui) identifier
               (sidebar-view-error ui) nil)
         (sidebar-cache-view ui identifier view)
         (terminal-ui-fullscreen-invalidate ui)
         t))
     (error (condition)
            (when (= epoch (sidebar-epoch ui))
              (setf (sidebar-view-error ui) (princ-to-string condition)))
            nil))))

(-> sidebar-refresh-view (sidebar-terminal-ui list real) t)


(defun sidebar-refresh-view (ui view now)
  "Tail a selected live trace at most once a second; preserve detached page scroll."
  (when (>= (- now (getf view :refreshed 0)) 1)
    (setf (getf view :refreshed) now)
    (handler-case
     (when
         (and (getf view :tail-p)
              (not
               (equal (getf view :signature)
                      (sidebar-trace-signature (getf view :path)
                                               (getf view :directory)))))
       (sidebar-load-page ui view (getf view :start) :tail-p t))
     (error (condition) (setf (getf view :error) (princ-to-string condition))))))

(-> sidebar-trace-frame (sidebar-terminal-ui list integer)
    (values list integer integer))


(defun sidebar-trace-frame (ui view width)
  "Compose normal conversation rows with a named tab and quiet read-only navigation."
  (let* ((trace-ui (getf view :ui))
         (viewport (fullscreen-terminal-ui-viewport trace-ui))
         (height (max 1 (terminal-rows (terminal-ui-terminal ui))))
         (space (max 0 (- height 2))))
    (let ((*sidebar-sizing-terminal* (terminal-ui-terminal ui))
          (*sidebar-sizing-width* width))
      (terminal-ui-fullscreen--ensure-width trace-ui width))
    (let* ((total (clinedi:transcript-viewport-row-count viewport))
           (top (clinedi:transcript-viewport-layout viewport space))
           (rows
            (loop for index from top below (min total (+ top space))
                  collect (clinedi:transcript-viewport-row-display viewport index))))
      (setf (getf view :top) top)
      (values
       (append (list (sidebar--session-heading ui (or (getf view :name) "agent") width))
               rows (make-list (max 0 (- space (length rows))) :initial-element "")
               (when (> height 1)
                 (list
                  (sidebar-cell
                   (or (getf view :error)
                       "read only   Esc main   PgUp/PgDn scroll   Home first   End live")
                   width))))
       (1- height) 0))))

(-> sidebar-view-event (sidebar-terminal-ui t) boolean)


(defun sidebar-view-event (ui event)
  "Consume every editor/submission event while inspecting another session."
  (let* ((view (sidebar-current-view ui))
         (trace-ui (getf view :ui))
         (viewport (fullscreen-terminal-ui-viewport trace-ui))
         (page-rows (max 1 (clinedi:transcript-viewport-page-rows viewport)))
         (top (getf view :top 0))
         (height (max 0 (- (terminal-rows (terminal-ui-terminal ui)) 2)))
         (total (clinedi:transcript-viewport-row-count viewport)))
    (handler-case
     (cond ((member event '(:escape :cancel)) (sidebar-select ui :primary))
           ((eq event :scroll-top) (sidebar-load-page ui view 1))
           ((eq event :scroll-bottom) (sidebar-load-page ui view 1 :tail-p t))
           ((and (eq event :page-up) (zerop top) (> (getf view :start) 1))
            (sidebar-load-page ui view
                               (max 1
                                    (- (getf view :start) *sidebar-trace-page-records*)))
            (clinedi:transcript-viewport-follow
             (fullscreen-terminal-ui-viewport trace-ui))
            (setf (getf view :tail-p) nil))
           ((and (eq event :page-down) (>= (+ top height) total) (getf view :more-p))
            (sidebar-load-page ui view (getf view :next)))
           ((and (consp event) (eq (first event) :click))
            (if (= (third event) 1)
                (sidebar-select ui :primary)
                (let ((action
                       (terminal-ui-fullscreen--click-action ui (second event)
                                                             (third event))))
                  (when action (setf (terminal-ui-pending-action ui) action)))))
           ((member event '(:page-up :page-down :previous-section :next-section))
            (setf (getf view :tail-p) nil)
            (clinedi:transcript-viewport-scroll viewport
                                                (if (member event
                                                            '(:page-up
                                                              :previous-section))
                                                    (- page-rows)
                                                    page-rows)))
           ((and (consp event) (eq (first event) :scroll)) (setf (getf view :tail-p) nil)
            (clinedi:transcript-viewport-scroll viewport (* 3 (second event)))))
     (error (condition) (setf (getf view :error) (princ-to-string condition))))
    (terminal-ui-fullscreen-invalidate ui)
    (terminal-ui--paint-live ui)
    t))

(-> sidebar-trace-signature (pathname &optional (option pathname)) list)


(defun sidebar-trace-signature (pathname &optional directory)
  "Fingerprint an owned append-only tail without decoding historical records."
  (let* ((segments
          (sidebar-owned-trace-segments (conversation-storage-identity-pathname pathname)
                                        directory))
         (tail (car (last segments))))
    (unless tail (sidebar--fail "The durable session trace is missing."))
    (list (namestring tail) (length segments) (file-write-date tail)
          (with-open-file (stream tail :element-type '(unsigned-byte 8))
            (file-length stream)))))


(define-condition sidebar-error
    (error)
    ((message :initarg :message :reader sidebar-error-message :documentation
      "The ownership, selection or trace failure."))
  (:report
   (lambda (condition stream) (write-string (sidebar-error-message condition) stream)))
  (:documentation "A recoverable failure inspecting an owned session."))

(-> sidebar--fail (string) nil)


(defun sidebar--fail (message)
  "Signal a typed session inspection failure with MESSAGE."
  (error 'sidebar-error :message message))
