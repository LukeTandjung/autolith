(in-package #:autolith)

;;;; -- Continuous Planner Fixtures --

(defstruct planner-test-script
  "Shared deterministic transcript for cloned planner/executor providers."
  (calls nil) (executor-rounds 0) (followups 2) failure
  (plan "Verify the last outcome, then continue the requested work."))

(defclass planner-test-provider (model-provider)
  ((configuration :initarg :configuration :reader provider-configuration
                  :documentation "The selected planner or executor model.")
   (script :initarg :script :reader planner-test-provider-script
           :documentation "The shared scripted response and request history."))
  (:documentation "A tool-free provider for deterministic native planner tests."))

(defmethod provider-with-configuration
    ((provider planner-test-provider) (configuration configuration))
  "Clone the selected model while sharing deterministic outcomes."
  (make-instance 'planner-test-provider :configuration configuration
                 :script (planner-test-provider-script provider)))

(-> planner-test--result (string &key (:completion keyword)) provider-result)
(defun planner-test--result (text &key (completion ':complete))
  "Return one ordinary metered assistant response."
  (make-instance 'provider-result :response-id "planner-test-response"
                 :output-items
                 (list (json-object "type" "message" "role" "assistant"
                                    "content" (vector (json-object "type" "output_text" "text" text))))
                 :tool-calls nil :usage (json-object "total_tokens" 3)
                 :turn-completion completion))

(defmethod provider-stream-turn
    ((provider planner-test-provider) (conversation conversation)
     &key tool-namespaces event-callback goal-context compaction-p)
  "Record the exact inference view before supplying a scripted outcome."
  (declare (ignore event-callback goal-context))
  (let* ((script (planner-test-provider-script provider))
         (planner-p (eql 0 (search "planner/" (conversation-prompt-cache-key conversation))))
         (snapshot (list :role (if planner-p ':planner ':executor)
                         :id (conversation-identifier conversation)
                         :key (conversation-prompt-cache-key conversation)
                         :inputs (mapcar #'json-encode (conversation-input-items conversation))
                         :tools (length tool-namespaces)
                         :hosted *provider-hosted-tools-enabled-p*
                         :compaction compaction-p)))
    (push snapshot (planner-test-script-calls script))
    (when (and planner-p (planner-test-script-failure script))
      (let ((failure (planner-test-script-failure script)))
        (setf (planner-test-script-failure script) nil)
        (error failure)))
    (if planner-p
        (planner-test--result (planner-test-script-plan script))
        (let ((round (incf (planner-test-script-executor-rounds script))))
          (planner-test--result
           (format nil "executor-outcome-~D" round)
           :completion (if (<= round (planner-test-script-followups script)) ':continue ':complete))))))

(-> planner-test--agent (configuration planner-test-script) agent)
(defun planner-test--agent (configuration script)
  "Create a primary recorded agent without starting workers or network traffic."
  (let* ((configuration (configuration-copy configuration :model "test/planner"
                                                         :provider-validation-p nil))
         (provider (make-instance 'planner-test-provider :configuration configuration :script script)))
    (make-instance 'agent :configuration configuration :provider provider
                   :conversation (conversation-create configuration)
                   :tool-registry (make-default-tool-registry :configuration configuration)
                   :worker nil)))

(-> planner-test--roles (planner-test-script) list)
(defun planner-test--roles (script)
  "Return inference roles in execution order."
  (mapcar (lambda (call) (getf call :role)) (reverse (planner-test-script-calls script))))

(-> planner-test--latest-delta (planner-state) string)
(defun planner-test--latest-delta (state)
  "Read the newest acknowledged delta from the durable planner transcript."
  (let ((text ""))
    (conversation-map-records
     (agent-conversation (planner-state-planner-agent state))
     (lambda (record)
       (when (and (eq (first record) ':message)
                  (eq (getf (getf (rest record) :operation-request) :kind) ':smart-planner-delta))
         (setf text (getf (rest record) :content)))))
    text))

;;;; -- Turn and Recovery Boundaries --

(-> test-planner-incremental-turn-and-restart () null)
(defun test-planner-incremental-turn-and-restart ()
  "Exercise real executor rounds, persistent prefixes and restart recovery."
  (with-test-configuration (configuration)
    (let* ((*planner-states* nil)
           (*planner-mode* ':off)
           (script (make-planner-test-script))
           (agent (planner-test--agent configuration script))
           (original-provider (agent-provider agent))
           (original-configuration (agent-configuration agent)))
      (planner-configure agent :mode ':on :executor "test/executor")
      (agent-run-user-turn agent "initial-task" :tools-p nil)
      (test-assert (equal '(:planner :executor :planner :executor :planner :executor)
                          (planner-test--roles script)) "planning precedes every executor inference")
      (test-assert (and (eq original-provider (agent-provider agent))
                        (eq original-configuration (agent-configuration agent)))
                   "routing bindings are restored after a complete turn")
      (let* ((state (planner-state-for agent (planner-settings (agent-conversation agent))))
             (conversation (agent-conversation (planner-state-planner-agent state)))
             (identifier (conversation-identifier conversation))
             (key (conversation-prompt-cache-key conversation))
             (calls (remove-if-not (lambda (call) (eq (getf call :role) ':planner))
                                   (reverse (planner-test-script-calls script))))
             (first-inputs (getf (first calls) :inputs))
             (second-inputs (getf (second calls) :inputs))
             (delta (planner-test--latest-delta state)))
        (test-assert (every (lambda (call) (and (zerop (getf call :tools))
                                                (not (getf call :hosted))
                                                (equal key (getf call :key)))) calls)
                     "planner requests have stable cache identity and no tools")
        (test-assert (equal first-inputs (subseq second-inputs 0 (length first-inputs)))
                     "incremental planning preserves the existing input prefix")
        (test-assert (and (search "executor-outcome-2" delta)
                          (not (search "executor-outcome-1" delta))
                          (not (search "initial-task" delta))
                          (not (search "Planning advice" delta)))
                     "deltas contain only new executor evidence, never prior advice")
        ;; Reload the primary as well as evicting the live pairing cache.
        (setf *planner-states* nil
              agent (make-instance
                     'agent :configuration (agent-configuration agent)
                     :provider (agent-provider agent) :worker (agent-worker agent)
                     :tool-registry (agent-tool-registry agent)
                     :conversation (conversation-load-by-id
                                    (agent-configuration agent)
                                    (conversation-identifier (agent-conversation agent)))))
        (let ((restored (planner-state-for agent (planner-settings (agent-conversation agent)))))
          (test-assert (and (equal identifier (conversation-identifier
                                              (agent-conversation (planner-state-planner-agent restored))))
                            (equal key (conversation-prompt-cache-key
                                       (agent-conversation (planner-state-planner-agent restored))))
                            (= (planner-state-cursor state) (planner-state-cursor restored))
                            (= (planner-state-revision state) (planner-state-revision restored)))
                       "disk reload restores planner history, identity and acknowledged checkpoint"))))))

(-> test-planner-pairing-and-bypass () null)
(defun test-planner-pairing-and-bypass ()
  "Exercise independent pairings, separate configurations and delegated bypass."
  (with-test-configuration (configuration)
    (let* ((*planner-states* nil)
           (*planner-mode* ':off)
           (script (make-planner-test-script :followups 0))
           (agent (planner-test--agent configuration script)))
      (planner-configure agent :mode ':on :executor "test/executor")
      (let* ((state (planner-state-for agent (planner-settings (agent-conversation agent))))
             (identifier (conversation-identifier (agent-conversation (planner-state-planner-agent state)))))
        (test-assert (and (not (eq (agent-configuration agent) (planner-state-executor-configuration state)))
                          (equal "test/executor" (config :model (planner-state-executor-configuration state))))
                     "executor receives a separate model configuration")
        (planner-configure agent :mode ':on :executor "test/executor-v2")
        (test-assert (not (equal identifier (conversation-identifier
                                           (agent-conversation (planner-state-planner-agent
                                                                (planner-state-for agent (planner-settings (agent-conversation agent))))))))
                     "model pairings keep independent planning histories")
        (planner-configure agent :mode ':on :executor "test/executor")
        (test-assert (equal identifier (conversation-identifier
                                       (agent-conversation (planner-state-planner-agent
                                                            (planner-state-for agent (planner-settings (agent-conversation agent)))))))
                     "switching back restores the original pairing")
        (let* ((other (planner-test--agent configuration (make-planner-test-script :followups 0)))
               (settings (list :mode ':on :executor "test/executor")))
          (test-assert (not (equal identifier (conversation-identifier
                                             (agent-conversation (planner-state-planner-agent
                                                                  (planner-state-for other settings))))))
                       "primary conversations cannot share planning history")))
      (planner-configure agent :mode ':off)
      (agent-run-user-turn agent "ordinary-task" :tools-p nil)
      (test-assert (equal '(:executor) (planner-test--roles script))
                   "OFF uses one ordinary inference without a planning history update")
      ;; The routing controller must reject children before touching any child-only slots.
      (let ((child (allocate-instance (find-class 'task-child-agent))))
        (setf (slot-value child 'conversation) (agent-conversation agent))
        (planner-configure child :mode ':on :executor "test/executor")
        (planner--run-user-turn child nil
                               (lambda ()
                                 (test-assert (null *planner-active-state*) "delegated agents bypass planning")))))))

(-> test-planner-failure-and-durable-acknowledgment () null)
(defun test-planner-failure-and-durable-acknowledgment ()
  "Preserve acknowledged deltas on cancellation and prevent executor inference."
  (with-test-configuration (configuration)
    (let* ((*planner-states* nil)
           (*planner-mode* ':off)
           (script (make-planner-test-script :followups 0))
           (agent (planner-test--agent configuration script))
           (original-provider (agent-provider agent)))
      (planner-configure agent :mode ':on :executor "test/executor")
      (dolist (condition (list (make-condition 'provider-error :message "script failure")
                               (make-condition 'application-turn-cancelled)))
        (setf (planner-test-script-failure script) condition)
        (test-assert (eq condition (handler-case
                                      (progn (agent-run-user-turn agent "failed-task" :tools-p nil) nil)
                                    (serious-condition (failure) failure)))
                     "planner failures propagate without replacement")
        (test-assert (and (zerop (planner-test-script-executor-rounds script))
                          (eq original-provider (agent-provider agent)))
                     "failure aborts before execution and restores original routing"))
      (let* ((state (planner-state-for agent (planner-settings (agent-conversation agent))))
             (cursor (planner-state-cursor state))
             (revision (planner-state-revision state)))
        (test-assert (and (plusp cursor) (= revision 2)) "failed requests still acknowledge their durable deltas")
        (setf *planner-states* nil)
        (let ((restored (planner-state-for agent (planner-settings (agent-conversation agent)))))
          (test-assert (and (= cursor (planner-state-cursor restored))
                            (= revision (planner-state-revision restored)))
                       "interrupted requests recover acknowledgment from the same delta record")))
      (agent-run-user-turn agent "retry-task" :tools-p nil)
      (let* ((state (planner-state-for agent (planner-settings (agent-conversation agent))))
             (delta (planner-test--latest-delta state)))
        (test-assert (and (search "retry-task" delta) (not (search "failed-task" delta)))
                     "retry sends only evidence newer than the failed request's acknowledgment")))))

(-> test-planner-bounds-and-shared-budget () null)
(defun test-planner-bounds-and-shared-budget ()
  "Exercise typed size failures, checkpoint validation and the combined request cap."
  (with-test-configuration (configuration)
    (let* ((*planner-states* nil)
           (*planner-mode* ':off)
           (script (make-planner-test-script :followups 0))
           (agent (planner-test--agent configuration script)))
      (planner-configure agent :mode ':on :executor "test/executor")
      (let ((*planner-maximum-delta-characters* 8))
        (test-assert (handler-case (progn (agent-run-user-turn agent "oversized-delta" :tools-p nil) nil)
                       (planner-error (condition) (eq ':delta-limit (planner-error-reason condition))))
                     "oversized deltas fail with a typed condition before inference"))
      (test-assert (null (planner-test-script-calls script)) "delta rejection issues no provider request")
      (dolist (plan '("" "long plan"))
        (setf (planner-test-script-plan script) plan)
        (let ((*planner-maximum-plan-characters* 4))
          (test-assert (handler-case (progn (agent-run-user-turn agent "bad-plan" :tools-p nil) nil)
                         (planner-error (condition) (eq ':plan-limit (planner-error-reason condition))))
                       "empty and oversized plans cannot authorize an executor request")))
      (test-assert (zerop (planner-test-script-executor-rounds script)) "invalid plans do not reach the executor")
      (setf (planner-test-script-plan script) "next")
      (let ((*agent-maximum-provider-requests-per-turn* 1))
        (test-assert (handler-case (progn (agent-run-user-turn agent "limited-task" :tools-p nil) nil)
                       (agent-loop-error () t))
                     "planner and executor share the per-turn request safety bound"))
      (test-assert (zerop (planner-test-script-executor-rounds script)) "the combined cap rejects execution after planning")
      (dolist (cursor '(-1 9223372036854775807))
        (let* ((restoration-agent (planner-test--agent configuration script))
               (settings (planner-configure restoration-agent :mode ':on))
               (state (planner-state-for restoration-agent settings))
               (conversation (agent-conversation (planner-state-planner-agent state))))
          (conversation-append-provider-metadata
           conversation (list :smart-planner-checkpoint (list :cursor cursor :revision 1)))
          (setf *planner-states* nil)
          (test-assert
           (handler-case (progn (planner-state-for restoration-agent settings) nil)
             (planner-error (condition) (eq ':checkpoint (planner-error-reason condition))))
           "negative or future checkpoints are rejected during durable restoration"))))))

(-> test-planner-mission-admission () null)
(defun test-planner-mission-admission ()
  "Keep mission and safety admission ahead of unissued executor requests."
  (with-test-configuration (configuration)
    (dolist (scenario '((:mission-turns 1 :request-limit 10 :condition mission-error)
                        (:mission-turns 10 :request-limit 1 :condition agent-loop-error)))
      (let* ((*planner-states* nil)
             (*planner-mode* ':off)
             (*mission-contexts* (make-hash-table :test #'eq))
             (*agent-maximum-provider-requests-per-turn* (getf scenario :request-limit))
             (script (make-planner-test-script :followups 0))
             (agent (planner-test--agent configuration script))
             (application (make-instance 'application :configuration (agent-configuration agent)
                                        :provider (agent-provider agent) :agent agent
                                        :conversation (agent-conversation agent)))
             (goal (list :kind ':mission :status ':active :deadline (+ (get-universal-time) 60)
                         :turn-limit (getf scenario :mission-turns) :turns-used 0
                         :token-limit 100000 :tokens-used 0 :tokens-reserved 0 :requests-outstanding 0)))
        (setf (application-goal application) goal)
        (mission--attach application)
        (planner-configure agent :mode ':on :executor "test/executor")
        (test-assert (typep (handler-case
                               (progn (agent-run-user-turn agent "mission-task" :tools-p nil) nil)
                             (mission-error (condition) condition)
                             (agent-loop-error (condition) condition))
                            (getf scenario :condition))
                     "the exhausted mission or safety allowance refuses executor admission")
        (test-assert (equal '(:planner) (planner-test--roles script))
                     "only the admitted planning request reaches its provider")
        (test-assert (and (= 1 (getf goal :turns-used)) (= 3 (getf goal :tokens-used))
                          (zerop (getf goal :tokens-reserved)) (zerop (getf goal :requests-outstanding)))
                     "unissued executor requests consume no mission turns or reservations")
        (let ((planner (planner-state-planner-agent
                        (planner-state-for agent (planner-settings (agent-conversation agent))))))
          (test-assert (and (null (mission-context-find agent))
                            (null (mission-context-find planner))
                            (null (mission-context-find (agent-conversation planner))))
                       "temporary mission bindings are removed after rejection"))))))


(-> test-planner-registration-and-command () null)
(defun test-planner-registration-and-command ()
  "Exercise cross-family factories and the registered native planner operation."
  (with-test-configuration (configuration)
    (let* ((*planner-states* nil)
           (*planner-mode* ':off)
           (*provider-registry* (provider-registry-create))
           (original-script (make-planner-test-script :followups 0))
           (selected-script (make-planner-test-script :followups 0))
           (agent (planner-test--agent configuration original-script))
           (application (make-instance 'application :configuration (agent-configuration agent)
                                      :provider (agent-provider agent) :agent agent
                                      :tool-registry (agent-tool-registry agent)
                                      :conversation (agent-conversation agent)))
           (presentations nil))
      (register-provider
       "planner-test-selected" :family ':planner-test
       :models '("test/cross-executor") :source ':runtime
       :factory (lambda (configuration &key reasoning-summaries-p)
                  (declare (ignore reasoning-summaries-p))
                  (make-instance 'planner-test-provider :configuration configuration
                                 :script selected-script)))
      (let* ((configuration (configuration-copy (agent-configuration agent)
                                               :model "test/cross-executor"))
             (selected (planner--selected-provider (agent-provider agent) configuration)))
        (test-assert (eq selected-script (planner-test-provider-script selected))
                     "cross-family selection calls the registered factory instead of copying the old adapter"))
      (let ((operation (application-operation-find application 'planner)))
        (test-assert (and (typep operation 'application-command-operation)
                          (application-operation-binding-eligible-p operation))
                     "planner is a normal callable application operation")
        (test-call-with-function-replacements
         (list (list 'application-present
                     (lambda (application spans)
                       (declare (ignore application))
                       (push spans presentations))))
         (lambda ()
           (test-assert (null (application-operation-invoke
                               operation application '("on" "test/cross-executor")))
                        "native invocation configures planning without starting inference")))
        (test-assert (and (eq ':on (getf (planner-settings (agent-conversation agent)) :mode))
                          (equal "test/cross-executor" (getf (planner-settings (agent-conversation agent)) :executor))
                          presentations
                          (null (planner-test-script-calls original-script))
                          (null (planner-test-script-calls selected-script)))
                     "the operation durably selects models and presents the selection without inference")))))
