(in-package #:autolith)


(defparameter *sidebar-record-limit* 256)


(defparameter *sidebar-frame-retention-seconds* 60)


(defvar *sidebar-lock*
  (make-lock "Private inference sidebar")
  "Live session rail *sidebar-lock* state.")


(defvar *sidebar-records*
  (make-hash-table :test 'equal)
  "Live session rail *sidebar-records* state.")


(defvar *sidebar-callbacks*
  (make-hash-table :test 'eq :weakness :key)
  "Live session rail *sidebar-callbacks* state.")


(defvar *sidebar-sequence* 0 "Live session rail *sidebar-sequence* state.")


(defvar *sidebar-owner* nil "Live session rail *sidebar-owner* state.")


(defvar *sidebar-trace-publisher*
  nil
  "Live session rail *sidebar-trace-publisher* state.")


(defvar *sidebar-frame-id* nil "Live session rail *sidebar-frame-id* state.")


(defvar *sidebar-sizing-terminal*
  nil
  "Live session rail *sidebar-sizing-terminal* state.")


(defvar *sidebar-sizing-width* nil "Live session rail *sidebar-sizing-width* state.")


(defclass sidebar-terminal-ui (fullscreen-terminal-ui)
          ((source :initform nil :accessor sidebar-source :documentation
            "Session rail source state." :type (option function))
           (owner :initform nil :accessor sidebar-ui-owner :documentation
            "Session rail owner state." :type (option string))
           (selected :initform nil :accessor sidebar-selected :documentation
            "Session rail selected state." :type (option string))
           (offset :initform 0 :accessor sidebar-offset :documentation
            "Session rail offset state." :type (integer 0))
           (hits :initform nil :accessor sidebar-hits :documentation
            "Session rail hits state." :type list)
           (main-width :initform nil :accessor sidebar-main-width :documentation
            "Session rail main-width state." :type (option integer))
           (panel-width :initform 0 :accessor sidebar-panel-width :documentation
            "Session rail panel-width state." :type (integer 0))
           (transcript :initform nil :accessor sidebar-transcript-function :documentation
            "Session rail transcript state." :type (option function))
           (application :initform nil :accessor sidebar-application :documentation
            "Session rail application state." :type (option application))
           (views :initform (make-hash-table :test 'equal) :accessor sidebar-views
            :documentation "Session rail views state." :type hash-table)
           (view-order :initform nil :accessor sidebar-view-order :documentation
            "Session rail view-order state." :type list)
           (linked-rows :initform (make-hash-table :test 'equal) :accessor
            sidebar-linked-rows :documentation "Session rail linked-rows state." :type
            hash-table)
           (epoch :initform 0 :accessor sidebar-epoch :documentation
            "Session rail epoch state." :type (integer 0))
           (error :initform nil :accessor sidebar-view-error :documentation
                  "Session rail error state." :type (option string))
           (workspace :initform nil :accessor sidebar-workspace :documentation
            "Last primary workspace identity, used to revoke cached inspection capabilities."
            :type (option string)))
          (:documentation
           "Left-hand owned sessions and isolated read-only trace viewports."))

(-> sidebar--rail-cell
    (sidebar-terminal-ui string integer &key (:marker (option string))
     (:selected boolean) (:muted boolean) (:accent boolean) (:state symbol))
    string)


(defun sidebar--rail-cell (ui text width &key marker selected muted accent state)
  "Pad a safe rail row with quiet selection and an optional coloured state dot."
  (let* ((width (max 1 width))
         (prefix
          (if marker
              (format nil " ~A " marker)
              " "))
         (prefix-width (clinedi:ansi-display-width prefix))
         (body-width (max 0 (- width prefix-width 1)))
         (body
          (if (plusp body-width)
              (sidebar-cell text body-width)
              ""))
         (plain (concatenate 'string prefix body " "))
         (foreground
          (cond (accent "192;159;239") (muted "111;115;139") (t "198;204;228")))
         (dot-colour
          (cond ((member state '(:failed :error :cancelled)) "231;130;137")
                ((member state '(:running :starting :tool :request :queued))
                 "145;180;248")
                (t "157;204;164")))
         (background
          (if selected
              "40;43;60"
              "30;30;40")))
    (cond ((< width (+ prefix-width 1)) (sidebar-cell "" width))
          ((terminal-styled-p (terminal-ui-terminal ui))
           (format nil "~C[48;2;~Am~C[38;2;~Am~A~C[38;2;~Am~A ~C[0m" #\Esc background
                   #\Esc
                   (if marker
                       dot-colour
                       foreground)
                   prefix #\Esc foreground body #\Esc))
          (t plain))))

(-> sidebar--grouped-rows (list) list)


(defun sidebar--grouped-rows (inventory)
  "Order sessions parent-first; cap indentation and tolerate cyclic ancestry."
  (let ((seen (make-hash-table :test 'equal)) (rows nil))
    (labels ((visit (item depth)
               (let ((identifier (getf item :id)))
                 (unless (gethash identifier seen)
                   (setf (gethash identifier seen) t)
                   (push (list item (min depth 2)) rows)
                   (dolist (child inventory)
                     (when (equal (getf child :parent) identifier)
                       (visit child (1+ depth))))))))
      (dolist (item inventory)
        (unless
            (find (getf item :parent) inventory :key
                  (lambda (candidate) (getf candidate :id)) :test #'equal)
          (visit item 0)))
      (dolist (item inventory) (visit item 0)))
    (nreverse rows)))

(-> sidebar--session-label (list integer) string)


(defun sidebar--session-label (item number)
  "Prefer a session's role or name to an opaque execution identifier."
  (or (getf item :name) (getf item :agent)
      (format nil "~A ~D"
              (case (getf item :kind)
                (:agent "agent")
                (:job "job")
                (:root "analysis")
                (:map "batch")
                (otherwise "analysis"))
              number)))

(-> sidebar--session-heading (sidebar-terminal-ui string integer) string)


(defun sidebar--session-heading (ui text width)
  "Fit the selected session tab to its label and one cell of padding per side."
  (let* ((width (max 0 width))
         (label
          (format nil " ~A "
                  (termdown:spans-text (list (terminal-span ':plain text)) :single-line-p
                                       t)))
         (tab-width (min width (clinedi:ansi-display-width label)))
         (visible
          (termdown:spans-text
           (terminal--clip-spans (list (terminal-span ':plain label)) tab-width)))
         (tab
          (concatenate 'string visible
                       (make-string
                        (max 0 (- tab-width (clinedi:ansi-display-width visible)))
                        :initial-element #\ )))
         (padding (make-string (- width tab-width) :initial-element #\ )))
    (if (and (plusp tab-width) (terminal-styled-p (terminal-ui-terminal ui)))
        (format nil "~C[48;2;145;180;248m~C[38;2;30;30;40m~A~C[0m~A" #\Esc #\Esc tab
                #\Esc padding)
        (concatenate 'string tab padding))))


(defmethod agent-observer-status :around ((observer agent-observer) status details)
  "Project canonical frame tools without arguments or activity prose."
  (when *sidebar-frame-id*
    (case status
      (:tool-call-started
       (sidebar-frame-update *sidebar-frame-id* :tool (getf details :tool) :state :tool))
      (:tool-call-completed
       (sidebar-frame-update *sidebar-frame-id* :tool "" :state :running))))
  (call-next-method))

(-> sidebar-compose (list list integer) list)


(defun sidebar-compose (main panel main-width)
  "Place padded panel rows before the transcript."
  (declare (ignore main-width))
  (loop for text in main
        for side in panel
        collect (concatenate 'string side text)))

(-> sidebar-panel-rows (sidebar-terminal-ui integer integer) list)


(defun sidebar-panel-rows (ui width height)
  "Draw workspace metadata and grouped sessions with two-line clickable rows."
  (let* ((inventory (sidebar--grouped-rows (sidebar-inventory ui)))
         (application (sidebar-application ui))
         (configuration
          (and application (slot-boundp application 'configuration)
               (application-configuration application)))
         (directory
          (if configuration
              (config :working-directory configuration)
              (uiop/os:getcwd)))
         (workspace (or (first (last (pathname-directory directory))) "workspace"))
         (branch (application--git-branch directory))
         (compact-p (< height 14))
         (step
          (if compact-p
              1
              2))
         (section
          (if compact-p
              0
              (max 5 (min (floor height 2) (- height (* 2 (1+ (length inventory))) 4)))))
         (primary-row
          (+ section
             (if compact-p
                 1
                 2)))
         (first-child (+ primary-row step))
         (capacity (max 0 (floor (- height first-child 1) step)))
         (start (min (sidebar-offset ui) (max 0 (- (length inventory) capacity))))
         (rows (make-array height :initial-element nil))
         (paged (> (length inventory) capacity)))
    (setf (sidebar-offset ui) start
          (sidebar-hits ui) nil)
    (labels ((put (index text &rest options)
               (when (< -1 index height)
                 (setf (aref rows index)
                         (apply #'sidebar--rail-cell ui text width options))))
             (hit (index identifier)
               (when (< -1 index height)
                 (push (cons (1+ index) identifier) (sidebar-hits ui)))))
      (unless compact-p
        (put 0 "spaces" :muted t)
        (put 2 (princ-to-string workspace) :marker "○")
        (put 3
         (if branch
             (format nil "  ~A" branch)
             "")
         :accent t)
        (hit 2 :primary)
        (hit 3 :primary))
      (put section "agents" :muted t)
      (put primary-row "main" :marker "○" :selected (null (sidebar-selected ui)))
      (hit primary-row :primary)
      (unless compact-p
        (put (1+ primary-row) "  autolith" :muted t :selected
         (null (sidebar-selected ui)))
        (hit (1+ primary-row) :primary))
      (loop for (item depth) in (subseq inventory start
                                        (min (length inventory) (+ start capacity)))
            for index from first-child by step
            for number from (1+ start)
            for selected = (equal (sidebar-selected ui) (getf item :id))
            for state = (getf item :state)
            for marker = (if (member state '(:running :queued :starting :tool :request))
                             "●"
                             "○")
            do (put index
                (format nil "~V@T~A" (* depth 2) (sidebar--session-label item number))
                :marker marker :selected selected :state state) (unless compact-p
                                                                  (put (1+ index)
                                                                   (format nil
                                                                           "  ~V@T~(~A~)"
                                                                           (* depth 2)
                                                                           state)
                                                                   :muted t :selected
                                                                   selected)) (unless
                                                                                  (eq
                                                                                   (getf
                                                                                    item
                                                                                    :kind)
                                                                                   :job)
                                                                                (hit
                                                                                 index
                                                                                 (getf
                                                                                  item
                                                                                  :id))
                                                                                (unless
                                                                                    compact-p
                                                                                  (hit
                                                                                   (1+
                                                                                    index)
                                                                                   (getf
                                                                                    item
                                                                                    :id)))))
      (when paged (put (1- height) "< prev    next >" :muted t))
      (loop for index below height
            collect (or (aref rows index) (sidebar--rail-cell ui "" width :muted t))))))

(-> sidebar-cell (string integer) string)


(defun sidebar-cell (text width)
  "Fit safe Unicode TEXT to one terminal row of WIDTH cells."
  (let* ((width (max 1 width))
         (safe
          (termdown:spans-text (list (terminal-span ':plain text)) :single-line-p t))
         (clipped-p (> (clinedi:ansi-display-width safe) width))
         (visible
          (termdown:spans-text
           (terminal--clip-spans (list (terminal-span ':plain safe))
                                 (if clipped-p
                                     (max 0 (1- width))
                                     width)))))
    (when clipped-p (setf visible (concatenate 'string visible "…")))
    (concatenate 'string visible
                 (make-string (max 0 (- width (clinedi:ansi-display-width visible)))
                              :initial-element #\ ))))

(-> sidebar-job-row (task-job) list)


(defun sidebar-job-row (job)
  "Project lifecycle metadata with a human-readable agent role or job name."
  (let* ((snapshot (session-job-snapshot job)) (progress (getf snapshot :progress)))
    (list :id (session-job-identifier job) :kind
          (if (typep job 'task-job)
              ':agent
              ':job)
          :name
          (or (getf snapshot :agent) (getf snapshot :description) (getf snapshot :tool))
          :parent (first (last (job-owner-identifiers job))) :state
          (getf snapshot :state) :tool
          (or (getf progress :current-tool) (getf snapshot :tool)) :requests
          (getf progress :request-count) :duration-ms (getf progress :duration-ms))))

(-> sidebar-layout-widths (integer) (values integer integer))


(defun sidebar-layout-widths (columns)
  "Reserve a narrow rail on wide terminals and at least 80 transcript columns."
  (let ((panel
         (if (>= columns 100)
             (min 28 (max 20 (floor (* columns 0.15))))
             0)))
    (values (- columns panel) panel)))

(-> sidebar-frame-snapshots (string) list)


(defun sidebar-frame-snapshots (owner)
  "Return the session rail's sidebar-frame-snapshots."
  (let ((now (terminal-ui--monotonic-seconds)))
    (with-lock-held (*sidebar-lock*)
      (sidebar-prune-records now)
      (sort
       (loop for row being the hash-values of *sidebar-records*
             when (equal owner (getf row :owner))
             collect (let ((copy (copy-list row)))
                       (setf (getf copy :duration-ms)
                               (round
                                (* 1000
                                   (- (or (getf copy :ended) now)
                                      (getf copy :started)))))
                       copy))
       #'< :key (lambda (row) (getf row :started))))))

(-> sidebar-call-with-frame (symbol (option function) function)
    (values &rest t))


(defun sidebar-call-with-frame (kind callback function)
  "Run FUNCTION with a bounded owned frame and an inherited activity callback."
  (let* ((lineage (sidebar-callback-lineage callback))
         (*sidebar-owner* (or *sidebar-owner* (first lineage)))
         (parent (or *sidebar-frame-id* (second lineage)))
         (now (terminal-ui--monotonic-seconds))
         (id
          (when *sidebar-owner*
            (with-lock-held (*sidebar-lock*)
              (sidebar-prune-records now :reserve-slot-p t)
              (let ((id (format nil "frame-~D" (incf *sidebar-sequence*))))
                (when (< (hash-table-count *sidebar-records*) *sidebar-record-limit*)
                  (setf (gethash id *sidebar-records*)
                          (list :id id :owner *sidebar-owner* :parent parent :kind kind
                                :state :running :started now :requests 0)))
                id))))
         (*sidebar-frame-id* id)
         (outcome :cancelled)
         (request-count 0)
         (activity
          (sidebar-register-callback
           (lambda (text)
             (when
                 (and id (equal id *sidebar-frame-id*) (stringp text)
                      (<= 8 (length text)) (string= "request " text :end2 8))
               (sidebar-frame-update id :requests (incf request-count)))
             (when callback (funcall callback text)))
           *sidebar-owner* *sidebar-frame-id*)))
    (unwind-protect
        (handler-bind ((error
                        (lambda (condition)
                          (declare (ignore condition))
                          (setf outcome :failed))))
          (multiple-value-call
              (lambda (&rest values)
                (setf outcome :completed)
                (when (and id (stringp (second values)))
                  (sidebar-frame-update id :trace (second values)))
                (values-list values))
            (funcall function activity)))
      (when id
        (with-lock-held (*sidebar-lock*)
          (let ((row (gethash id *sidebar-records*)))
            (when row
              (setf (getf row :state) outcome
                    (getf row :ended) (terminal-ui--monotonic-seconds)
                    (getf row :tool) nil
                    (gethash id *sidebar-records*) row)))
          (sidebar-prune-records (terminal-ui--monotonic-seconds)))
        (sidebar-publish-frame-end id)))))

(-> sidebar-frame-update
    (string &key (:requests (option integer)) (:tool (option string))
            (:state symbol) (:trace (option string)))
    (option list))


(defun sidebar-frame-update (id &key requests tool state trace)
  "Store typed counters and canonical identities; publish owned trace links once."
  (let ((link nil))
    (with-lock-held (*sidebar-lock*)
      (let ((row (gethash id *sidebar-records*)))
        (when row
          (when (and (integerp requests) (<= 0 requests 1000000))
            (setf (getf row :requests) requests))
          (when tool (setf (getf row :tool) (sidebar-identifier tool)))
          (when state (setf (getf row :state) state))
          (when (and trace (not (equal trace (getf row :trace))))
            (setf (getf row :trace) (sidebar-identifier trace)
                  link (copy-list row)))
          (setf (gethash id *sidebar-records*) row))))
    (when (and link *sidebar-trace-publisher*) (funcall *sidebar-trace-publisher* link)))
  nil)

(-> sidebar-callback-lineage ((option function)) list)


(defun sidebar-callback-lineage (callback)
  "Return the session rail's sidebar-callback-lineage."
  (with-lock-held (*sidebar-lock*)
    (copy-list (gethash callback *sidebar-callbacks*))))

(-> sidebar-register-callback
    ((option function) (option string) (option string)) (option function))


(defun sidebar-register-callback (callback owner parent)
  "Return the session rail's sidebar-register-callback."
  (when callback
    (with-lock-held (*sidebar-lock*)
      (setf (gethash callback *sidebar-callbacks*) (list owner parent))))
  callback)

(-> sidebar-prune-records (real &key (:reserve-slot-p boolean)) null)


(defun sidebar-prune-records (now &key reserve-slot-p)
  "Caller holds SIDEBAR-LOCK. Evict only terminal records, never active frames."
  (let ((terminal nil))
    (maphash
     (lambda (id row)
       (when (getf row :ended)
         (if (> (- now (getf row :ended)) *sidebar-frame-retention-seconds*)
             (remhash id *sidebar-records*)
             (push row terminal))))
     *sidebar-records*)
    (dolist (row (sort terminal #'< :key (lambda (row) (getf row :ended))))
      (when
          (if reserve-slot-p
              (>= (hash-table-count *sidebar-records*) *sidebar-record-limit*)
              (> (hash-table-count *sidebar-records*) *sidebar-record-limit*))
        (remhash (getf row :id) *sidebar-records*)))))

(-> sidebar-identifier (t &optional integer) (option string))


(defun sidebar-identifier (value &optional (limit 64))
  "Bound an identifier to printable ASCII; control and Unicode text become spaces."
  (let ((text (typecase value (string value) (symbol (symbol-name value)) (t "?"))))
    (map 'string
         (lambda (character)
           (if (<= 32 (char-code character) 126)
               character
               #\ ))
         (subseq text 0 (min limit (length text))))))

(-> sidebar-canonical-frame (list) list)


(defun sidebar-canonical-frame (row)
  "Project durable inference identities and canonical parent links without retaining prompt data."
  (let ((copy (copy-list row)))
    (when (getf copy :trace)
      (setf (getf copy :frame-id) (getf copy :id)
            (getf copy :id) (format nil "inference:~A" (getf copy :trace))
            (getf copy :name) (format nil "analysis ~A" (getf copy :trace))))
    (let ((parent (getf copy :parent)))
      (when parent
        (let ((trace
               (with-lock-held (*sidebar-lock*)
                 (getf (gethash parent *sidebar-records*) :trace))))
          (when trace (setf (getf copy :parent) (format nil "inference:~A" trace))))))
    copy))

(-> sidebar-connect-trace-publisher (application sidebar-terminal-ui) function)


(defun sidebar-connect-trace-publisher (application ui)
  "Persist owned trace identities outside request history, under native presentation locks."
  (setf *sidebar-trace-publisher*
          (lambda (row)
            (with-recursive-lock-held ((application-render-lock application))
              (when
                  (and (slot-boundp application 'conversation)
                       (slot-boundp application 'agent) (application-agent application)
                       (equal (getf row :owner)
                              (task-parent-root-conversation-identifier
                               (application-agent application))))
                (let ((link (sidebar-canonical-frame row)))
                  (conversation-append-provider-metadata
                   (application-conversation application)
                   (list :sidebar-trace-link
                         (list :id (getf link :id) :trace (getf link :trace) :owner
                               (getf link :owner) :parent (getf link :parent) :kind
                               (getf link :kind) :name (getf link :name) :state
                               (getf link :state))))
                  (with-terminal-ui-locked (ui)
                    (when (equal (sidebar-ui-owner ui) (getf link :owner))
                      (setf (gethash (getf link :id) (sidebar-linked-rows ui))
                              link)))))))))

(-> sidebar-restore-trace-links (sidebar-terminal-ui (option string)) t)


(defun sidebar-restore-trace-links (ui owner)
  "Recover owned inference links when attaching or switching the primary root."
  (let ((application (sidebar-application ui)))
    (when
        (and owner application (slot-boundp application 'conversation)
             (conversation-persisted-p (application-conversation application)))
      (conversation-map-records (application-conversation application)
                                (lambda (record)
                                  (let ((link
                                         (and (eq (first record) :provider)
                                              (getf (getf (rest record) :metadata)
                                                    :sidebar-trace-link))))
                                    (when
                                        (and link (equal owner (getf link :owner))
                                             (stringp (getf link :id))
                                             (rlm--trace-identifier-p
                                              (getf link :trace)))
                                      (setf (gethash (getf link :id)
                                                     (sidebar-linked-rows ui))
                                              link))))))))

(-> sidebar-publish-frame-end (string) t)


(defun sidebar-publish-frame-end (id)
  "Persist the final state for a previously linked inference session."
  (let ((row
         (with-lock-held (*sidebar-lock*)
           (copy-list (gethash id *sidebar-records*)))))
    (when (and row (getf row :trace) *sidebar-trace-publisher*)
      (funcall *sidebar-trace-publisher* row))))


(defmethod rlm-call-with-observation :around ((kind symbol) callback function)
  (sidebar-call-with-frame kind callback
                           (lambda (activity) (call-next-method kind activity function))))


(defmethod rlm-note-conversation :around ((conversation conversation))
  (when *sidebar-frame-id*
    (sidebar-frame-update *sidebar-frame-id* :trace
                          (conversation-identifier conversation)))
  (call-next-method))


(defmethod rlm-inherit-activity-callback :around (callback &key parent viewer)
  (let ((lineage (sidebar-callback-lineage parent)))
    (sidebar-register-callback (call-next-method)
                               (or *sidebar-owner* (first lineage)
                                   (and viewer
                                        (task-parent-root-conversation-identifier
                                         viewer)))
                               (or *sidebar-frame-id* (second lineage)))))


(defmethod terminal-columns :around ((terminal terminal))
  "Use the pane width while composing an isolated session frame."
  (if (eq terminal *sidebar-sizing-terminal*)
      *sidebar-sizing-width*
      (call-next-method)))


(defmethod terminal-ui-fullscreen--ensure-width :around ((ui sidebar-terminal-ui) width)
  "Keep streamed rows and frame composition at the same pane width."
  (call-next-method ui
   (or
    (and (eq (terminal-ui-terminal ui) *sidebar-sizing-terminal*) *sidebar-sizing-width*)
    (nth-value 0
               (sidebar-layout-widths
                (max 1 (terminal-columns (terminal-ui-terminal ui))))))))


(defmethod terminal-ui-activate :around ((ui sidebar-terminal-ui) action)
  "Select session actions without modifying the primary transcript."
  (if (eq (first action) :sidebar-transcript)
      (sidebar-select ui (second action))
      (call-next-method)))


(defmethod terminal-ui-fullscreen--frame
           ((ui sidebar-terminal-ui) status-now &optional requested-width)
  "Compose the rail with a native primary or isolated child conversation."
  (declare (ignore requested-width))
  (multiple-value-bind (main-width panel-width)
      (sidebar-layout-widths (max 1 (terminal-columns (terminal-ui-terminal ui))))
    (sidebar-inventory ui)
    (setf (sidebar-main-width ui) main-width
          (sidebar-panel-width ui) panel-width)
    (let ((view (sidebar-current-view ui)))
      (when view (sidebar-refresh-view ui view status-now))
      (multiple-value-bind (rows cursor-row cursor-column)
          (if view
              (sidebar-trace-frame ui view main-width)
              (let* ((terminal (terminal-ui-terminal ui))
                     (*sidebar-sizing-terminal* terminal)
                     (*sidebar-sizing-width* main-width)
                     (height (terminal-rows terminal))
                     (heading-p (> height 1)))
                (unwind-protect
                    (progn
                     (setf (terminal-rows terminal)
                             (max 1
                                  (- height
                                     (if heading-p
                                         1
                                         0))))
                     (multiple-value-bind (rows row column)
                         (call-next-method ui status-now main-width)
                       (values
                        (if heading-p
                            (cons
                             (sidebar--session-heading ui
                                                       (or (sidebar-view-error ui)
                                                           "main")
                                                       main-width)
                             rows)
                            rows)
                        (+ row
                           (if heading-p
                               1
                               0))
                        column)))
                  (setf (terminal-rows terminal) height))))
        (unless (plusp panel-width) (setf (sidebar-hits ui) nil))
        (values
         (if (plusp panel-width)
             (sidebar-compose rows (sidebar-panel-rows ui panel-width (length rows))
                              main-width)
             rows)
         cursor-row (+ panel-width cursor-column))))))


(defmethod terminal-ui-fullscreen-handle-event ((ui sidebar-terminal-ui) event)
  "Route rail clicks, paging and trace navigation without changing the main draft."
  (multiple-value-bind (main-width panel-width)
      (sidebar-layout-widths (max 1 (terminal-columns (terminal-ui-terminal ui))))
    (setf (sidebar-main-width ui) main-width
          (sidebar-panel-width ui) panel-width)
    (unless (plusp panel-width) (setf (sidebar-hits ui) nil)))
  (cond
   ((and (typep ui 'sidebar-terminal-ui) (consp event) (eq (first event) :click)
         (<= 1 (second event) (sidebar-panel-width ui)))
    (let ((hit (assoc (third event) (sidebar-hits ui))))
      (cond (hit (sidebar-select ui (rest hit)))
            ((and (= (third event) (terminal-rows (terminal-ui-terminal ui)))
                  (plusp (sidebar-panel-width ui)))
             (incf (sidebar-offset ui)
                   (if (< (second event) (ceiling (sidebar-panel-width ui) 2))
                       -1
                       1))
             (setf (sidebar-offset ui) (max 0 (sidebar-offset ui))))))
    (terminal-ui--paint-live ui) t)
   ((and (typep ui 'sidebar-terminal-ui) (sidebar-current-view ui))
    (sidebar-view-event ui event) t)
   ((and (typep ui 'sidebar-terminal-ui) (consp event) (eq (first event) :click))
    (let ((action (terminal-ui-fullscreen--click-action ui (second event) (third event))))
      (when action (setf (terminal-ui-pending-action ui) action)))
    t)
   (t (call-next-method ui event))))


(defmethod terminal-ui-fullscreen--click-action ((ui sidebar-terminal-ui) column row)
  "Translate rail and heading coordinates before native widget hit testing."
  (let ((offset (sidebar-panel-width ui)))
    (when (and (> column offset) (> row 1))
      (let ((view (sidebar-current-view ui)))
        (if view
            (terminal-ui-fullscreen--click-action (getf view :ui) (- column offset)
                                                  (1- row))
            (call-next-method ui (- column offset) (1- row)))))))

(-> sidebar-attach (application) (option sidebar-terminal-ui))


(defun sidebar-attach (application)
  "Connect the native fullscreen session rail to APPLICATION's owned jobs and trace resolver."
  (let ((ui (and (slot-boundp application 'ui) (application-ui application))))
    (when (typep ui 'fullscreen-terminal-ui)
      (unless (typep ui 'sidebar-terminal-ui) (change-class ui 'sidebar-terminal-ui))
      (unless (eq (sidebar-application ui) application)
        (setf (sidebar-application ui) application)
        (sidebar-connect-trace-publisher application ui))
      (setf (sidebar-source ui)
              (lambda ()
                (let ((viewer
                       (and (slot-boundp application 'agent)
                            (application-agent application)))
                      (orchestrator (application--task-orchestrator application)))
                  (when (and viewer orchestrator)
                    (mapcar #'sidebar-job-row
                            (remove-if-not (lambda (job) (typep job 'task-job))
                                           (task-orchestrator-list-visible-jobs
                                            orchestrator viewer))))))
            (sidebar-transcript-function ui)
              (lambda (identifier) (sidebar-resolve-trace application ui identifier)))
      ui)))

(-> sidebar-inventory (sidebar-terminal-ui) list)


(defun sidebar-inventory (ui)
  "Merge owned task sessions and durable inference links after revoking stale workspace capabilities."
  (let* ((application (sidebar-application ui))
         (viewer
          (and application (slot-boundp application 'agent)
               (application-agent application)))
         (owner (and viewer (task-parent-root-conversation-identifier viewer)))
         (workspace
          (and application (slot-boundp application 'configuration)
               (namestring
                (config :working-directory (application-configuration application))))))
    (when
        (and application
             (or (not (equal owner (sidebar-ui-owner ui)))
                 (not (equal workspace (sidebar-workspace ui)))))
      (setf (sidebar-workspace ui) workspace)
      (sidebar-revoke ui owner)))
  (let* ((jobs
          (when (sidebar-source ui)
            (remove-if (lambda (row) (eq (getf row :kind) :job))
                       (funcall (sidebar-source ui)))))
         (frames
          (when (sidebar-ui-owner ui) (sidebar-frame-snapshots (sidebar-ui-owner ui))))
         (live (mapcar #'sidebar-canonical-frame frames)))
    (append jobs live
            (loop for row being the hash-values of (sidebar-linked-rows ui)
                  unless (find (getf row :id) live :key (lambda (item) (getf item :id))
                               :test #'equal)
                  collect row))))
