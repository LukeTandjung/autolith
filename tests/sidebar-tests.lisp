(in-package #:autolith)


(defun test-sidebar-owned-conversations ()
  "Exercise native ownership, durable inference links, live task traces and bounded view caching."
  (with-test-configuration (configuration)
    (let ((*sidebar-records* (make-hash-table :test 'equal))
          (*sidebar-callbacks* (make-hash-table :test 'eq))
          (*sidebar-trace-publisher* nil)
          (*sidebar-test-checks* 0))
      (sidebar-test-owned-durable-traces configuration)
      (sidebar-test-live-task-traces configuration)
      (sidebar-test-primary-scroll-and-trace-hits))))


(defvar *sidebar-test-checks* 0)


(defmacro sidebar-check (form)
  "Record one native session behavior assertion."
  `(progn (test-assert ,form ,(prin1-to-string form)) (incf *sidebar-test-checks*)))


(defmacro sidebar-test-functions (bindings &body body)
  (let ((saved (gensym "SAVED")))
    `(let ((,saved
            (mapcar (lambda (name) (cons name (symbol-function name)))
                    ',(mapcar #'first bindings))))
       (unwind-protect
           (progn
            ,@(mapcar
               (lambda (binding)
                 `(setf (symbol-function ',(first binding)) ,(second binding)))
               bindings)
            ,@body)
         (dolist (pair ,saved) (setf (symbol-function (car pair)) (cdr pair)))))))


(defclass sidebar-test-job (task-job)
          ((snapshot :initarg :snapshot :accessor sidebar-test-snapshot)))


(defmethod session-job-snapshot ((job sidebar-test-job))
  (copy-tree (sidebar-test-snapshot job)))


(defun sidebar-test-ui (&optional (columns 120) (rows 24) (fullscreen-p t))
  "Construct or inspect a synthetic session sidebar fixture."
  (terminal-ui-create :terminal
                      (make-instance 'terminal :columns columns :rows rows :interactive-p
                                     nil :styled-p nil)
                      :fullscreen-p fullscreen-p))


(defun sidebar-test-application (ui)
  "Construct or inspect a synthetic session sidebar fixture."
  (let ((application (allocate-instance (find-class 'application))))
    (setf (slot-value application 'ui) ui
          (slot-value application 'agent) nil
          (slot-value application 'render-lock)
            (make-recursive-lock "Synthetic sidebar application"))
    application))


(defun sidebar-test-viewport-text (ui)
  "Construct or inspect a synthetic session sidebar fixture."
  (let* ((viewport (fullscreen-terminal-ui-viewport ui))
         (count (clinedi:transcript-viewport-row-count viewport)))
    (format nil "~{~A~%~}"
            (loop for row below
                  count
                  collect (clinedi:transcript-viewport-row-display viewport row)))))


(defun sidebar-test-substring-count (needle haystack)
  "Construct or inspect a synthetic session sidebar fixture."
  (loop with start = 0
        for match = (search needle haystack :start2 start)
        while match
        count match
        do (setf start (+ match (length needle)))))


(defun sidebar-test-owned-durable-traces (configuration)
  "Verify native child ownership, live RLM links, and metadata reconstruction."
  (let* ((*sidebar-trace-publisher* nil)
         (primary (conversation-create configuration :identifier "owner-a"))
         (trace
          (conversation-create configuration :identifier "rlm-trace" :storage-root
                               (configuration-inference-root configuration)))
         (agent (allocate-instance (find-class 'agent)))
         (ui (sidebar-test-ui))
         (application (sidebar-test-application ui)))
    (setf (slot-value agent 'conversation) primary
          (slot-value application 'agent) agent
          (slot-value application 'conversation) primary
          (slot-value application 'configuration) configuration)
    (conversation-append-user-message primary "primary")
    (conversation-append-user-message trace "RLM-DURABLE-BODY")
    (sidebar-test-functions ((application--task-orchestrator
                              (lambda (application) (declare (ignore application)) nil)))
      (sidebar-attach application)
      (sidebar-inventory ui)
      (let ((*sidebar-owner* "owner-a") id)
        (sidebar-call-with-frame :rlm nil
                                 (lambda (callback)
                                   (declare (ignore callback))
                                   (setf id *sidebar-frame-id*)
                                   (rlm-note-conversation trace)
                                   (sidebar-check
                                    (equal "rlm-trace"
                                           (getf (gethash id *sidebar-records*) :trace)))
                                   (let ((fingerprint
                                          (sidebar-test-conversation-fingerprint
                                           primary)))
                                     (sidebar-check
                                      (sidebar-select ui "inference:rlm-trace"))
                                     (sidebar-check
                                      (search "RLM-DURABLE-BODY"
                                              (sidebar-test-viewport-text
                                               (getf (sidebar-current-view ui) :ui))))
                                     (sidebar-view-event ui :scroll-top)
                                     (sidebar-view-event ui :scroll-bottom)
                                     (sidebar-check
                                      (equal fingerprint
                                             (sidebar-test-conversation-fingerprint
                                              primary))))
                                   (values :done "rlm-trace")))
        (sidebar-check (eq :completed (getf (gethash id *sidebar-records*) :state))))
      (clrhash *sidebar-records*)
      (sidebar-revoke ui "owner-a")
      (sidebar-check
       (find "inference:rlm-trace" (sidebar-inventory ui) :key
             (lambda (row) (getf row :id)) :test #'equal))
      (let ((fingerprint (sidebar-test-conversation-fingerprint primary)))
        (sidebar-check (sidebar-select ui "inference:rlm-trace"))
        (sidebar-check
         (eq :completed
             (getf (gethash "inference:rlm-trace" (sidebar-linked-rows ui)) :state)))
        (setf (getf (gethash "inference:rlm-trace" (sidebar-linked-rows ui)) :kind)
                :root)
        (sidebar-check (sidebar-select ui "inference:rlm-trace"))
        (sidebar-check
         (equal fingerprint (sidebar-test-conversation-fingerprint primary))))
      (sidebar-revoke ui "different-owner")
      (sidebar-check (zerop (hash-table-count (sidebar-linked-rows ui))))
      (sidebar-check (null (sidebar-current-view ui))))))


(defun sidebar-test-primary-scroll-and-trace-hits ()
  "Test a detached primary scroll and selected-trace actions across a prepaint resize."
  (let* ((ui (sidebar-test-ui))
         (terminal (terminal-ui-terminal ui))
         (primary (fullscreen-terminal-ui-viewport ui))
         (trace-ui (sidebar-test-ui))
         (trace (fullscreen-terminal-ui-viewport trace-ui))
         (text (format nil "copy https://example.com~%"))
         (view
          (list :ui trace-ui :name "Trace coordinates" :top 0 :start 1 :next 2 :more-p
                nil :tail-p nil :refreshed 0 :owner "owner-a")))
    (change-class ui 'sidebar-terminal-ui)
    (setf (sidebar-ui-owner ui) "owner-a")
    (line-editor-set-text (terminal-ui-editor ui) (make-string 95 :initial-element #\d))
    (clinedi:transcript-viewport-append primary
                                        (format nil "~{~A~%~}"
                                                (make-list 50 :initial-element
                                                           "primary line"))
                                        (format nil "~{~A~%~}"
                                                (make-list 50 :initial-element
                                                           "primary line")))
    (terminal-ui-fullscreen--frame ui 0)
    (clinedi:transcript-viewport-scroll-to-top primary)
    (clinedi:transcript-viewport-scroll primary 5)
    (terminal-ui-fullscreen--frame ui 0)
    (let ((top (clinedi:transcript-viewport-layout primary 19)))
      (sidebar-check (= 5 top))
      (clinedi:transcript-viewport-append trace text text :regions
                                          (list (list 0 4 '(:copy "trace code"))))
      (clinedi:transcript-viewport-scroll-to-top trace)
      (setf (gethash "coords" (sidebar-views ui)) view
            (sidebar-selected ui) "coords")
      (terminal-ui-fullscreen--frame ui 0)
      (sidebar-check
       (equal '(:copy "trace code")
              (terminal-ui-fullscreen--click-action ui (+ 2 (sidebar-panel-width ui))
                                                    2)))
      (sidebar-check
       (equal '(:open-url "https://example.com")
              (terminal-ui-fullscreen--click-action ui (+ 8 (sidebar-panel-width ui))
                                                    2)))
      (sidebar-check (null (terminal-ui-fullscreen--click-action ui 2 2)))
      (terminal-ui-fullscreen-handle-event ui
                                           (list :click (+ 2 (sidebar-panel-width ui))
                                                 2))
      (sidebar-check (equal '(:copy "trace code") (terminal-ui-pending-action ui)))
      (setf (terminal-ui-pending-action ui) nil
            (slot-value terminal 'clinedi::columns) 90)
      (terminal-ui-fullscreen-handle-event ui '(:click 8 2))
      (sidebar-check (= 0 (sidebar-panel-width ui)))
      (sidebar-check
       (equal '(:open-url "https://example.com") (terminal-ui-pending-action ui)))
      (setf (terminal-ui-pending-action ui) nil)
      (sidebar-view-event ui :escape)
      (terminal-ui-fullscreen--frame ui 0)
      (sidebar-check (null (sidebar-selected ui)))
      (sidebar-check (eq primary (fullscreen-terminal-ui-viewport ui)))
      (sidebar-check (= top (clinedi:transcript-viewport-layout primary 19)))
      (sidebar-check
       (equal (make-string 95 :initial-element #\d)
              (line-editor-text (terminal-ui-editor ui))))
      (sidebar-test-functions ((sidebar-restore-trace-links
                                (lambda (&rest args)
                                  (declare (ignore args))
                                  (error "synthetic link failure"))))
        (sidebar-revoke ui "owner-b"))
      (sidebar-check (null (sidebar-current-view ui)))
      (sidebar-check (search "synthetic link failure" (sidebar-view-error ui))))))


(defun sidebar-test-conversation-fingerprint (conversation)
  "Capture exact durable records, including provider and recovery metadata."
  (list (slot-value conversation 'next-sequence)
        (mapcar #'uiop/stream:read-file-string
                (conversation-storage-pathnames (conversation-pathname conversation)))))


(defun sidebar-test-admit-trace-job (orchestrator parent index)
  "Admit a real native task job without starting providers or worker threads.

Inline-only admission retains a live queued job in the native pool. Only the
scheduler's initialized task fields are supplied here; resolution and snapshot
projection use the production implementations without stubs."
  (let* ((execution (format nil "trace-execution-~D" index))
         (job
          (cl-jobpond:job-pool-submit (task-orchestrator-pool orchestrator)
                                      (lambda (job) (declare (ignore job)) nil) :name
                                      (format nil "trace-child-~D" index) :inline-only-p
                                      t :root-identifier
                                      (task-parent-root-conversation-identifier parent))))
    (setf (slot-value job 'orchestrator) orchestrator
          (slot-value job 'execution-identifier) execution
          (slot-value job 'definition) nil
          (slot-value job 'definition-summary) '(:name "worker" :source :builtin)
          (slot-value job 'item) '(:agent "worker" :task "Synthetic trace inspection")
          (slot-value job 'parent-agent) parent)
    (let* ((continuity (task-continuity-record-job job parent))
           (directory (uiop/pathname:pathname-directory-pathname continuity))
           (conversation
            (conversation-create (agent-configuration parent) :identifier execution
                                 :storage-root directory)))
      (conversation-append-user-message conversation (format nil "OWNED-CHILD-~D" index))
      (values job conversation directory))))


(defun sidebar-test-live-task-traces (configuration)
  "Exercise the native resolver, bounded cache, and realpath task ownership."
  (let* ((*configuration-store* nil)
         (*sidebar-view-cache-limit* 4)
         (primary (conversation-create configuration :identifier "native-trace-owner"))
         (parent (allocate-instance (find-class 'agent)))
         (ui (sidebar-test-ui))
         (application (sidebar-test-application ui))
         (registry (make-instance 'tool-registry))
         (orchestrator (task-orchestrator-create))
         (jobs nil)
         (conversations nil)
         (directories nil))
    (setf (slot-value parent 'configuration) configuration
          (slot-value parent 'conversation) primary
          (slot-value application 'configuration) configuration
          (slot-value application 'agent) parent
          (slot-value application 'conversation) primary
          (slot-value application 'tool-registry) registry)
    (tool-registry-register registry
                            (make-instance 'task-run-tool :namespace "task" :name "run"
                                           :orchestrator orchestrator))
    (conversation-append-user-message primary "PRIMARY-NATIVE-TRACE-TEST")
    (conversation-append-provider-metadata primary
                                           '(:synthetic-recovery-marker "unchanged"))
    (unwind-protect
        (progn
         (dotimes (index 12)
           (multiple-value-bind (job conversation directory)
               (sidebar-test-admit-trace-job orchestrator parent index)
             (push job jobs)
             (push conversation conversations)
             (push directory directories)))
         (setf jobs (nreverse jobs)
               conversations (nreverse conversations)
               directories (nreverse directories))
         (sidebar-attach application)
         (sidebar-check (eq orchestrator (application--task-orchestrator application)))
         (sidebar-check
          (every (lambda (job) (eq (class-of job) (find-class 'task-job))) jobs))
         (sidebar-check
          (every (lambda (job) (eq :queued (getf (session-job-snapshot job) :state)))
                 jobs))
         (let* ((first-id (session-job-identifier (first jobs)))
                (row
                 (find first-id (sidebar-inventory ui) :key (lambda (row) (getf row :id))
                       :test #'equal))
                (fingerprint (sidebar-test-conversation-fingerprint primary))
                (editor (terminal-ui-editor ui))
                (viewport (fullscreen-terminal-ui-viewport ui)))
           (line-editor-set-text editor "PRIMARY CACHE DRAFT")
           (sidebar-check (eq :agent (getf row :kind)))
           (sidebar-check (sidebar-select ui first-id))
           (sidebar-check
            (equal (conversation-pathname (first conversations))
                   (getf (sidebar-current-view ui) :path)))
           (sidebar-check
            (search "OWNED-CHILD-0"
                    (sidebar-test-viewport-text (getf (sidebar-current-view ui) :ui))))
           (let ((first-view (sidebar-current-view ui)))
             (dolist (job (rest jobs))
               (sidebar-check (sidebar-select ui (session-job-identifier job)))
               (sidebar-check
                (<= (hash-table-count (sidebar-views ui)) *sidebar-view-cache-limit*))
               (sidebar-check
                (eq (sidebar-current-view ui)
                    (gethash (session-job-identifier job) (sidebar-views ui)))))
             (sidebar-check (null (gethash first-id (sidebar-views ui))))
             (sidebar-check
              (= *sidebar-view-cache-limit* (length (sidebar-view-order ui))))
             (sidebar-check (sidebar-select ui first-id))
             (sidebar-check (not (eq first-view (sidebar-current-view ui))))
             (sidebar-check
              (search "OWNED-CHILD-0"
                      (sidebar-test-viewport-text
                       (getf (sidebar-current-view ui) :ui)))))
           (let* ((recent-id (session-job-identifier (car (last jobs))))
                  (recent (gethash recent-id (sidebar-views ui))))
             (sidebar-check (sidebar-select ui recent-id))
             (sidebar-check (eq recent (sidebar-current-view ui))))
           (sidebar-view-event ui :scroll-top)
           (sidebar-view-event ui :page-down)
           (sidebar-view-event ui :scroll-bottom)
           (sidebar-select ui :primary)
           (sidebar-check
            (= *sidebar-view-cache-limit* (hash-table-count (sidebar-views ui))))
           (sidebar-check (eq editor (terminal-ui-editor ui)))
           (sidebar-check (eq viewport (fullscreen-terminal-ui-viewport ui)))
           (sidebar-check (equal "PRIMARY CACHE DRAFT" (line-editor-text editor)))
           (sidebar-check
            (equal fingerprint (sidebar-test-conversation-fingerprint primary)))
           (let* ((job (first jobs))
                  (directory (first directories))
                  (outside
                   (conversation-create configuration :identifier
                                        "outside-task-artifact"))
                  (owned (first conversations)))
             (conversation-append-user-message outside "OUTSIDE-SECRET")
             (setf (slot-value job 'cl-jobpond::result)
                     (list :conversation-file
                           (namestring (conversation-pathname outside))))
             (sidebar-check (null (sidebar-select ui first-id)))
             (sidebar-check (null (sidebar-selected ui)))
             (sidebar-check (search "owned transcript" (sidebar-view-error ui)))
             (sidebar-check
              (handler-case
               (progn
                (sidebar-read-page (conversation-pathname outside) 1 nil directory)
                nil)
               (task-continuity-error nil t)))
             (sidebar-check
              (handler-case
               (progn
                (sidebar-trace-signature (conversation-pathname outside) directory)
                nil)
               (task-continuity-error nil t)))
             (let ((result-path (merge-pathnames "result.sexp" directory)))
               (task-continuity--write result-path
                                       (list :status :success :conversation-file
                                             (namestring
                                              (conversation-pathname outside))))
               (setf (slot-value job 'cl-jobpond::result) nil)
               (sidebar-check (null (sidebar-select ui first-id)))
               (sidebar-check (search "owned transcript" (sidebar-view-error ui)))
               (delete-file result-path))
             (let ((alias (merge-pathnames "outside-alias.sexp" directory)))
               (uiop/run-program:run-program
                (list "/bin/ln" "-s"
                      (namestring
                       (first
                        (conversation-storage-pathnames
                         (conversation-pathname outside))))
                      (namestring alias)))
               (setf (slot-value job 'cl-jobpond::result)
                       (list :conversation-file (namestring alias)))
               (sidebar-check (null (sidebar-select ui first-id)))
               (sidebar-check (search "owned transcript" (sidebar-view-error ui)))
               (sidebar-check
                (handler-case (progn (sidebar-trace-signature alias directory) nil)
                              (task-continuity-error nil t)))
               (delete-file alias))
             (setf (slot-value job 'cl-jobpond::result)
                     (list :conversation-file
                           (namestring (conversation-pathname owned))))
             (sidebar-check (sidebar-select ui first-id))
             (sidebar-check
              (search "OWNED-CHILD-0"
                      (sidebar-test-viewport-text (getf (sidebar-current-view ui) :ui))))
             (let* ((alternate
                     (conversation-create configuration :identifier "alternate-trace"
                                          :storage-root directory))
                    (old-view (sidebar-current-view ui)))
               (conversation-append-user-message alternate "ALTERNATE-OWNED-TRACE")
               (setf (slot-value job 'cl-jobpond::result)
                       (list :conversation-file
                             (namestring (conversation-pathname alternate))))
               (sidebar-check (sidebar-select ui first-id))
               (sidebar-check (not (eq old-view (sidebar-current-view ui))))
               (sidebar-check
                (search "ALTERNATE-OWNED-TRACE"
                        (sidebar-test-viewport-text
                         (getf (sidebar-current-view ui) :ui))))
               (setf (slot-value job 'cl-jobpond::result)
                       (list :conversation-file
                             (namestring (conversation-pathname owned))))
               (sidebar-check (sidebar-select ui first-id)))
             (let* ((view (sidebar-current-view ui))
                    (old (fullscreen-terminal-ui-viewport (getf view :ui))))
               (setf (getf view :path) (conversation-pathname outside))
               (sidebar-view-event ui :scroll-top)
               (sidebar-check (search "owned transcript" (getf view :error)))
               (sidebar-check (eq old (fullscreen-terminal-ui-viewport (getf view :ui))))
               (setf (getf view :path) (conversation-pathname owned)))
             (sidebar-check
              (equal fingerprint (sidebar-test-conversation-fingerprint primary)))))
         (sidebar-revoke ui "next-owner")
         (sidebar-check (null (sidebar-view-order ui)))
         (sidebar-check (zerop (hash-table-count (sidebar-views ui)))))
      (job-pool-close (task-orchestrator-pool orchestrator))
      (job-pool-close (task-orchestrator-execution-pool orchestrator)))))


(defvar *sidebar-redesign-checks* 0)


(defvar *sidebar-redesign-ui* nil)


(defvar *sidebar-redesign-root* nil)


(defmacro sidebar-redesign-check (form)
  "Record one native full-frame or event-path assertion."
  `(progn (test-assert ,form ,(prin1-to-string form)) (incf *sidebar-redesign-checks*)))


(defun sidebar-redesign-fixture
       (columns rows &key styled-p (working-directory *sidebar-redesign-root*))
  "Create a synthetic primary session with no credential, network or startup effects."
  (let* ((configuration
          (configuration-create :durable-p nil :defer-provider-validation-p t
                                :working-directory working-directory :data-root
                                *sidebar-redesign-root* :compact-view-p t
                                :reasoning-traces-p nil))
         (terminal
          (make-instance 'terminal :columns columns :rows rows :interactive-p nil
                         :styled-p styled-p))
         (ui (terminal-ui-create :terminal terminal :fullscreen-p t))
         (application (allocate-instance (find-class 'application)))
         (registry (make-instance 'tool-registry)))
    (default-tools--register-lisp registry)
    (change-class ui 'sidebar-terminal-ui)
    (setf (slot-value application 'configuration) configuration
          (slot-value application 'ui) ui
          (slot-value application 'tool-registry) registry
          (sidebar-application ui) application
          (sidebar-ui-owner ui) "synthetic-owner"
          (sidebar-source ui)
            (lambda ()
              (list '(:id "review" :name "reviewer" :kind :agent :state :completed)
                    '(:id "research" :name "researcher" :kind :agent :state :running)
                    '(:id "nested" :name "verifier" :parent "research" :kind :agent
                      :state :running)
                    '(:id "tool" :name "build" :kind :job :state :running))))
    ui))


(defun test-sidebar-full-frames ()
  "Exercise full frames, child selection, paging, compact rendering and draft isolation."
  (unwind-protect
      (progn
       (let ((*configuration-store* nil)
             (*sidebar-records* (make-hash-table :test 'equal))
             (*sidebar-trace-publisher* nil))
         (setf *sidebar-redesign-checks* 0
               *sidebar-redesign-root*
                 (ensure-directories-exist
                  (merge-pathnames (format nil "sidebar-redesign-~A/" (make-identifier))
                                   (uiop/stream:temporary-directory))))
         (dolist (columns '(60 99 100 120 168 240))
           (dolist (height '(1 3 8 13 14 24 48))
             (dolist (styled-p '(nil t))
               (let ((ui (sidebar-redesign-fixture columns height :styled-p styled-p)))
                 (multiple-value-bind (rows cursor-row cursor-column)
                     (terminal-ui-fullscreen--frame ui 0)
                   (sidebar-redesign-check (= height (length rows)))
                   (sidebar-redesign-check
                    (every (lambda (row) (<= (clinedi:ansi-display-width row) columns))
                           rows))
                   (sidebar-redesign-check (<= 0 cursor-row (1- height)))
                   (sidebar-redesign-check (<= 0 cursor-column columns))
                   (when (>= columns 100)
                     (sidebar-redesign-check (>= (sidebar-main-width ui) 80))))))))
         (let* ((items
                 '((:id "child" :parent "parent") (:id "parent")
                   (:id "cycle-a" :parent "cycle-b") (:id "cycle-b" :parent "cycle-a")))
                (grouped (sidebar--grouped-rows items)))
           (sidebar-redesign-check (= 4 (length grouped)))
           (sidebar-redesign-check (equal "parent" (getf (first (first grouped)) :id)))
           (sidebar-redesign-check (= 1 (second (second grouped)))))
         (let* ((ui (sidebar-redesign-fixture 168 48 :styled-p t))
                (configuration (application-configuration (sidebar-application ui)))
                (conversation (conversation-create configuration :identifier "research"))
                (draft "Untouched primary draft")
                (editor (terminal-ui-editor ui))
                (primary-viewport (fullscreen-terminal-ui-viewport ui)))
           (setf *sidebar-redesign-ui* ui
                 (sidebar-transcript-function ui)
                   (lambda (identifier)
                     (when
                         (member identifier '("research" "nested" "review") :test
                                 #'equal)
                       (list :path (conversation-pathname conversation) :name
                             "researcher" :owner "synthetic-owner"))))
           (line-editor-set-text editor draft)
           (clinedi:transcript-viewport-append primary-viewport
                                               (format nil "Primary conversation~%")
                                               (format nil "Primary conversation~%"))
           (dotimes (index 300)
             (conversation-append-user-message conversation
                                               (format nil "Research step ~D" index)))
           (conversation-append-provider-item conversation
                                              (json-object "type" "function_call" "name"
                                                           "lisp.eval" "call_id"
                                                           "synthetic-call" "arguments"
                                                           (json-encode
                                                            (json-object "repl"
                                                                         "research"
                                                                         "forms"
                                                                         (json-array
                                                                          (format nil
                                                                                  "(loop repeat 100~%  collect (list :deep~%    (list :nested (make-list 100))))"))))))
           (conversation-append-tool-result conversation "synthetic-call" :tool-name
                                            "lisp.eval" :success-p t :output
                                            (format nil
                                                    "Values:~%  (:RESULTS 12 :VERIFIED T)"))
           (conversation-append-provider-item conversation
                                              (json-object "type" "message" "role"
                                                           "assistant" "content"
                                                           (json-array
                                                            (json-object "type"
                                                                         "output_text"
                                                                         "text"
                                                                         (format nil
                                                                                 "## Research complete~%~%Found twelve relevant products.~%~%- Compared prices and availability.~%- Checked the source listings.~%~%The verified results are ready for review.")))))
           (terminal-ui-fullscreen--frame ui 0)
           (let* ((hits (sidebar-hits ui))
                  (first-row
                   (first
                    (sort
                     (loop for (row . identifier) in hits
                           when (equal identifier "research")
                           collect row)
                     #'<))))
             (sidebar-redesign-check (integerp first-row))
             (terminal-ui-fullscreen-handle-event ui (list :click 3 (1+ first-row)))
             (sidebar-redesign-check (equal "research" (sidebar-selected ui))))
           (let* ((view (sidebar-current-view ui))
                  (renderer (sidebar-renderer ui (getf view :ui)))
                  (reasoning
                   (list :provider-item :wire-json
                         (json-encode
                          (json-object "type" "reasoning" "summary"
                                       (json-array
                                        (json-object "type" "summary_text" "text"
                                                     "Hidden reasoning")))))))
             (sidebar-redesign-check
              (not
               (eq primary-viewport (fullscreen-terminal-ui-viewport (getf view :ui)))))
             (sidebar-redesign-check (null (sidebar-trace-entry renderer reasoning)))
             (sidebar-redesign-check
              (null (sidebar-trace-entry renderer '(:internal :nested (1 2 3)))))
             (sidebar-redesign-check
              (sidebar-trace-entry renderer
                                   '(:message :role :assistant :content
                                     "Readable persisted answer")))
             (sidebar-redesign-check (> (getf view :start) 100))
             (sidebar-view-event ui :scroll-top)
             (sidebar-redesign-check (= 1 (getf view :start)))
             (sidebar-view-event ui :scroll-bottom)
             (sidebar-redesign-check (> (getf view :start) 100))
             (dolist (event '(:submit :complete (:insert "wrong") (:paste "wrong")))
               (sidebar-view-event ui event))
             (sidebar-redesign-check (string= draft (line-editor-text editor)))
             (multiple-value-bind (rows cursor-row cursor-column)
                 (terminal-ui-fullscreen--frame ui 0)
               (declare (ignore cursor-row cursor-column))
               (sidebar-redesign-check (= 48 (length rows)))
               (sidebar-redesign-check
                (every (lambda (row) (<= (clinedi:ansi-display-width row) 168)) rows)))
             (let ((terminal (terminal-ui-terminal ui)))
               (dolist (columns '(60 99 100 120 168 240))
                 (dolist (height '(1 3 8 13 14 24 48))
                   (setf (terminal-columns terminal) columns
                         (terminal-rows terminal) height)
                   (multiple-value-bind (rows row column)
                       (terminal-ui-fullscreen--frame ui 0)
                     (sidebar-redesign-check (= height (length rows)))
                     (sidebar-redesign-check
                      (every
                       (lambda (text) (<= (clinedi:ansi-display-width text) columns))
                       rows))
                     (sidebar-redesign-check (<= 0 row (1- height)))
                     (sidebar-redesign-check (<= 0 column columns)))))
               (setf (terminal-columns terminal) 168
                     (terminal-rows terminal) 48))
             (sidebar-view-event ui :escape)
             (sidebar-redesign-check (null (sidebar-selected ui)))
             (sidebar-redesign-check (eq editor (terminal-ui-editor ui)))
             (sidebar-redesign-check
              (eq primary-viewport (fullscreen-terminal-ui-viewport ui)))
             (sidebar-redesign-check (string= draft (line-editor-text editor)))
             (setf (sidebar-offset ui) 1000)
             (terminal-ui-fullscreen--frame ui 0)
             (sidebar-redesign-check (zerop (sidebar-offset ui)))
             (sidebar-select ui "research")))
         (let ((ui (sidebar-redesign-fixture 120 24)))
           (setf (sidebar-source ui)
                   (lambda ()
                     (loop for index below 40
                           collect (list :id (format nil "session-~D" index) :name
                                         "worker" :kind ':agent :state ':running))))
           (terminal-ui-fullscreen--frame ui 0)
           (let ((before (sidebar-offset ui)))
             (terminal-ui-fullscreen-handle-event ui '(:click 19 24))
             (sidebar-redesign-check (= (1+ before) (sidebar-offset ui)))
             (terminal-ui-fullscreen-handle-event ui '(:click 2 24))
             (sidebar-redesign-check (= before (sidebar-offset ui)))))
         (dolist (styled-p '(nil t))
           (let ((ui (sidebar-redesign-fixture 168 48 :styled-p styled-p)))
             (dolist
                 (label
                  '("" "main" "reviewer" "a-session-label-longer-than-twenty-eight-cells"
                    "研究" "café" "👩‍💻"))
               (dolist (width '(0 1 2 5 10 28 60 140))
                 (let* ((heading (sidebar--session-heading ui label width))
                        (label-width (+ 2 (clinedi:ansi-display-width label)))
                        (reset (search (format nil "~C[0m" #\Esc) heading)))
                   (sidebar-redesign-check
                    (= width (clinedi:ansi-display-width heading)))
                   (when (>= width label-width)
                     (sidebar-redesign-check (search (format nil " ~A " label) heading)))
                   (when (and styled-p (plusp width))
                     (sidebar-redesign-check reset)
                     (sidebar-redesign-check
                      (= (min width label-width)
                         (clinedi:ansi-display-width (subseq heading 0 reset))))))))))
         (list :checks *sidebar-redesign-checks* :full-render-path t :child-selection t
               :draft-preserved t :network-requests 0)))
    (when *sidebar-redesign-root*
      (platform-delete-directory-tree *platform* *sidebar-redesign-root* :validate t
                                      :if-does-not-exist ':ignore))))


(defun test-sidebar-queued-completion-priority ()
  "Cycle queued drafts past passive suggestions, but retain explicit completion selection."
  (let* ((terminal (make-instance 'recording-terminal :columns 80 :interactive-p t))
         (ui (terminal-ui-create
              :terminal terminal
              :completions
              '((:name "/resume" :argument "ID" :description "resume conversation")
                (:name "/rollback" :argument "ID" :description "select generation")))))
    (terminal-ui-set-input ui "/r")
    (test-assert (not (terminal-ui-completion-active-p ui))
                 "suggestions are initially passive")
    (multiple-value-bind (action draft)
        (terminal-ui-process-event ui :complete-previous :queue-editing-p t)
      (test-assert (eq action ':cycle-queue) "queued draft takes priority")
      (test-assert (equal draft "/r") "queue cycling preserves the draft"))
    (terminal-ui-set-input ui "/r")
    (terminal-ui-process-event ui :complete :queue-editing-p t)
    (test-assert (terminal-ui-completion-active-p ui) "Tab explicitly selects completion")
    (multiple-value-bind (action payload)
        (terminal-ui-process-event ui :complete-previous :queue-editing-p t)
      (declare (ignore payload))
      (test-assert (not (eq action ':cycle-queue)) "explicit completion takes priority")
      (test-assert (terminal-ui-completion-active-p ui) "completion remains active"))))


(defun test-sidebar-native-observation ()
  "Observe inherited inference callbacks and terminal outcomes through native lifecycle hooks."
  (let ((*sidebar-records* (make-hash-table :test 'equal))
        (*sidebar-callbacks* (make-hash-table :test 'eq))
        (*sidebar-trace-publisher* nil)
        (*sidebar-owner* "owner")
        (*sidebar-frame-id* nil)
        (parent nil)
        (child nil))
    (test-assert
     (equal
      '(:complete 7)
      (multiple-value-list
       (rlm-call-with-observation
        ':root nil
        (lambda (callback)
          (setf parent *sidebar-frame-id*)
          (funcall callback "request 1")
          (let ((inherited (rlm-inherit-activity-callback
                            (lambda (text) (declare (ignore text))) :parent callback)))
            (test-assert (equal (sidebar-callback-lineage inherited)
                                (list "owner" parent)) "callback lineage is inherited")
            (handler-case
                (rlm-call-with-observation
                 ':rlm inherited
                 (lambda (activity)
                   (setf child *sidebar-frame-id*)
                   (funcall activity "request 1")
                   (error 'sidebar-error :message "failed inference")))
              (sidebar-error () nil)))
          (values ':complete 7)))))
     "lifecycle observation preserves all return values")
    (let ((parent-row (gethash parent *sidebar-records*))
          (child-row (gethash child *sidebar-records*)))
      (test-assert (eq (getf parent-row :state) ':completed) "parent completes")
      (test-assert (eq (getf child-row :state) ':failed) "child failure is terminal")
      (test-assert (equal (getf child-row :parent) parent) "nested frame names its parent")
      (test-assert (= 1 (getf parent-row :requests)) "nested requests do not inflate parent")
      (test-assert (= 1 (getf child-row :requests)) "child request count is observed")
      (test-assert (and (getf parent-row :ended) (getf child-row :ended))
                   "both frames finish during unwind"))))
