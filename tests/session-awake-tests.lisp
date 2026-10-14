(in-package #:autolith)

;;;; -- Sleep-Inhibitor Lifecycle Tests --

(defclass session-awake-test-preferences (setinka:setting-store) ()
  (:documentation "A preference snapshot with explicit durable longevity optouts."))

(defmethod setinka:store-read-values
    ((store session-awake-test-preferences) configuration)
  "Return durable optouts without reading or writing personal preferences."
  (declare (ignore store configuration))
  '(:persistent-sessions-p nil :prevent-session-sleep-p nil))

(defclass session-awake-test-process ()
  ((alive-p :initarg :alive-p :accessor session-awake-test-process-alive-p
            :documentation "Synthetic helper liveness.")
   (waits :initform 0 :accessor session-awake-test-process-waits
          :documentation "Number of reaps after confirmed exit."))
  (:documentation "A deterministic owned helper without an operating-system process."))

(defclass session-awake-test-platform (platform)
  ((launches :initform 0 :accessor session-awake-test-platform-launches
             :documentation "Number of requested helper launches.")
   (processes :initform nil :accessor session-awake-test-platform-processes
              :documentation "Launched helpers, newest first.")
   (startup-alive-p :initform t :accessor session-awake-test-platform-startup-alive-p
                    :documentation "Whether new helpers survive startup.")
   (launch-failure-p :initform nil :accessor session-awake-test-platform-launch-failure-p
                     :documentation "Whether launching signals an expected host failure.")
   (termination-mode :initform ':normal
                     :accessor session-awake-test-platform-termination-mode
                     :documentation "Whether TERM works, only force works, or neither works.")
   (terminations :initform nil :accessor session-awake-test-platform-terminations
                 :documentation "Ordered force flags from termination requests."))
  (:documentation "Mock sleep helper backend with controlled startup and disposal failures."))

(defmethod platform-sleep-inhibitor-backend ((platform session-awake-test-platform))
  "Identify the test helper backend."
  (declare (ignore platform))
  ':test)

(defmethod platform-sleep-inhibitor-command
    ((platform session-awake-test-platform) owner-process-id)
  "Represent PID binding without executing a host command."
  (declare (ignore platform))
  (list "synthetic-helper" (format nil "~D" owner-process-id)))

(defmethod platform-launch-sleep-inhibitor
    ((platform session-awake-test-platform) command)
  "Launch a synthetic helper or signal a controlled host refusal."
  (declare (ignore command))
  (incf (session-awake-test-platform-launches platform))
  (when (session-awake-test-platform-launch-failure-p platform)
    (error 'platform-capability-unavailable :capability ':sleep-inhibition
           :message "Synthetic launch refusal."))
  (let ((process (make-instance 'session-awake-test-process
                               :alive-p (session-awake-test-platform-startup-alive-p platform))))
    (push process (session-awake-test-platform-processes platform))
    process))

(defmethod platform-process-object-alive-p
    ((platform session-awake-test-platform) (process session-awake-test-process))
  "Observe synthetic helper liveness."
  (declare (ignore platform))
  (session-awake-test-process-alive-p process))

(defmethod platform-terminate-process-object
    ((platform session-awake-test-platform) (process session-awake-test-process) &key force)
  "Apply the selected helper termination behavior."
  (push force (session-awake-test-platform-terminations platform))
  (when (or (eq (session-awake-test-platform-termination-mode platform) ':normal)
            (and force (eq (session-awake-test-platform-termination-mode platform) ':force)))
    (setf (session-awake-test-process-alive-p process) nil))
  nil)

(defmethod platform-wait-process
    ((platform session-awake-test-platform) (process session-awake-test-process))
  "Reap only an exited synthetic helper."
  (declare (ignore platform))
  (test-assert (not (session-awake-test-process-alive-p process))
               "helper reaping requires confirmed exit")
  (incf (session-awake-test-process-waits process)))

(-> session-awake-test--call-with-state (function) t)
(defun session-awake-test--call-with-state (function)
  "Run FUNCTION with isolated global state visible to real monitor threads."
  (let* ((symbols '(*platform* *session-awake-process* *session-awake-owner-pid*
                    *session-awake-backend* *session-awake-monitor*
                    *session-awake-monitor-token* *session-awake-failure*
                    *session-awake-lock* *session-awake-wakeup*
                    *session-awake-check-seconds* sb-ext:*init-hooks* sb-ext:*exit-hooks*))
         (saved (mapcar (lambda (symbol) (cons symbol (symbol-value symbol))) symbols)))
    (unwind-protect
         (progn
           (session-awake--after-restore)
           (setf *platform* (make-instance 'session-awake-test-platform)
                 *session-awake-check-seconds* 0.02
                 sb-ext:*init-hooks* nil
                 sb-ext:*exit-hooks* nil)
           (funcall function))
      (setf (session-awake-test-platform-termination-mode *platform*) ':normal)
      (session-awake-release)
      (dolist (entry saved)
        (setf (symbol-value (first entry)) (rest entry))))))

(-> test-session-awake-policy () null)
(defun test-session-awake-policy ()
  "Exercise interactive startup policy, optout, restore hooks and exit cleanup."
  (let ((configuration (make-configuration :provider-validation-p nil))
        (disabled (setinka:configuration-load
                   :store (make-instance 'session-awake-test-preferences)
                   :environment-p nil :validation-deferred-p t)))
    (test-assert (and (config :persistent-sessions-p configuration)
                      (config :prevent-session-sleep-p configuration))
                 "new sessions default to persistence and sleep prevention")
    (test-assert (and (null (config :persistent-sessions-p disabled))
                      (null (config :prevent-session-sleep-p disabled))
                      (eq (configuration-setting-source disabled :persistent-sessions-p)
                          ':durable)
                      (eq (configuration-setting-source disabled :prevent-session-sleep-p)
                          ':durable))
                 "durable NIL optouts take precedence over native defaults")
    (session-awake-test--call-with-state
     (lambda ()
       (session-longevity-install configuration)
       (session-longevity-install configuration :interactive-p t :pristine-p t)
       (test-assert (zerop (session-awake-test-platform-launches *platform*))
                    "headless and pristine startup never acquire helpers")
       (session-longevity-install disabled :interactive-p t)
       (test-assert (zerop (session-awake-test-platform-launches *platform*))
                    "explicit sleep-prevention NIL is respected")
       (session-longevity-install configuration :interactive-p t)
       (session-longevity-install configuration :interactive-p t)
       (test-assert (= 1 (session-awake-test-platform-launches *platform*))
                    "repeated interactive installation owns only one helper")
       (test-assert (= 1 (count 'session-awake--after-restore sb-ext:*init-hooks*))
                    "restore invalidation is registered once")
       (test-assert (= 1 (count 'session-awake-release sb-ext:*exit-hooks*))
                    "exit cleanup is registered once")
       (let ((process *session-awake-process*)
             (monitor *session-awake-monitor*))
         (session-longevity-install disabled :interactive-p t)
         (test-assert (and (null *session-awake-process*)
                           (not (session-awake-test-process-alive-p process))
                           (not (thread-alive-p monitor)))
                      "sleep-prevention optout releases an already owned helper"))
       (session-longevity-install configuration :interactive-p t)
       (let ((process *session-awake-process*)
             (monitor *session-awake-monitor*))
         (funcall (first sb-ext:*exit-hooks*))
         (test-assert (and (not (session-awake-test-process-alive-p process))
                           (= 1 (session-awake-test-process-waits process))
                           (not (thread-alive-p monitor)))
                      "exit cleanup terminates and reaps helper and joins monitor")))))
  nil)

(-> test-session-awake-recovery () null)
(defun test-session-awake-recovery ()
  "Exercise startup failures, recoverable optout and bounded helper replacement."
  (session-awake-test--call-with-state
   (lambda ()
     (setf (session-awake-test-platform-launch-failure-p *platform*) t)
     (handler-bind ((session-awake-unavailable
                     (lambda (condition)
                       (test-assert (typep (session-awake-unavailable-cause condition)
                                           'platform-capability-unavailable)
                                    "launch failure retains structured host cause")
                       (invoke-restart 'continue-without-inhibitor))))
       (test-assert (null (session-awake-acquire)) "host refusal permits explicit optout"))
     (setf (session-awake-test-platform-launch-failure-p *platform*) nil
           (session-awake-test-platform-startup-alive-p *platform*) nil)
     (test-assert
      (handler-case (progn (session-awake-acquire) nil)
        (session-awake-unavailable () t))
      "early helper exit is detected before publication")
     (test-assert (and (null *session-awake-process*)
                       (= 1 (session-awake-test-process-waits
                             (first (session-awake-test-platform-processes *platform*)))))
                  "failed startup reaps the unpublished helper")
     (setf (session-awake-test-platform-startup-alive-p *platform*) t)
     (test-assert (eq ':test (session-awake-acquire)) "explicit retry acquires helper")
     (let ((first *session-awake-process*))
       (setf (session-awake-test-process-alive-p first) nil)
       (multiple-value-bind (continue-p retries-left)
           (session-awake--monitor-check (current-process-id) 1)
         (test-assert (and continue-p (zerop retries-left)
                           (not (eq first *session-awake-process*))
                           (= 1 (session-awake-test-process-waits first)))
                      "first helper loss is reaped and replaced once")
         (setf (session-awake-test-process-alive-p *session-awake-process*) nil)
         (test-assert (not (session-awake--monitor-check (current-process-id) retries-left))
                      "second helper loss exhausts automatic retry")
         (test-assert (typep *session-awake-failure* 'session-awake-unavailable)
                      "asynchronous failure remains observable")
         (session-awake-acquire)
         (test-assert (null *session-awake-failure*) "explicit recovery clears failure")))))
  nil)

(-> test-session-awake-monitor () null)
(defun test-session-awake-monitor ()
  "Exercise the real monitor thread and release wakeup with a mock host backend."
  (session-awake-test--call-with-state
   (lambda ()
     (session-awake-acquire)
     (let ((process *session-awake-process*)
           (monitor (session-awake-monitor-start)))
       (test-assert (eq monitor (session-awake-monitor-start))
                    "only one liveness monitor runs")
       (with-lock-held (*session-awake-lock*)
         (setf (session-awake-test-process-alive-p process) nil)
         (condition-notify *session-awake-wakeup*))
       (test-assert
        (loop repeat 100
              when (with-lock-held (*session-awake-lock*)
                     (not (eq process *session-awake-process*)))
                return t
              do (sleep 0.01))
        "monitor replaces an exited helper")
       (setf process *session-awake-process*)
       (session-awake-release)
       (test-assert (and (not (thread-alive-p monitor))
                         (not (session-awake-test-process-alive-p process))
                         (= 1 (session-awake-test-process-waits process)))
                    "release wakes and joins monitor before returning"))))
  nil)

(-> test-session-awake-restore-and-disposal () null)
(defun test-session-awake-restore-and-disposal ()
  "Exercise inherited-handle invalidation and retryable bounded disposal failures."
  (session-awake-test--call-with-state
   (lambda ()
     (session-awake-acquire)
     (let ((inherited *session-awake-process*)
           (old-lock *session-awake-lock*)
           (old-wakeup *session-awake-wakeup*))
       (session-awake--after-restore)
       (test-assert (and (null *session-awake-process*)
                         (null *session-awake-monitor*)
                         (not (eq old-lock *session-awake-lock*))
                         (not (eq old-wakeup *session-awake-wakeup*))
                         (session-awake-test-process-alive-p inherited)
                         (zerop (session-awake-test-process-waits inherited)))
                    "restore replaces synchronization objects without touching inherited handles")
       (session-awake-acquire)
       (test-assert (not (eq inherited *session-awake-process*))
                    "restored process acquires its own helper")
       (session-awake-release)
       (setf *session-awake-process* inherited
             *session-awake-owner-pid* (1+ (current-process-id)))
       (session-awake-release)
       (test-assert (and (session-awake-test-process-alive-p inherited)
                         (zerop (session-awake-test-process-waits inherited)))
                    "owner mismatch forgets but never signals or waits on inherited handle"))
     (session-awake-acquire)
     (let ((process *session-awake-process*))
       (setf (session-awake-test-platform-termination-mode *platform*) ':refuse)
       (test-assert
        (handler-case (progn (session-awake-release) nil)
          (session-awake-unavailable () t))
        "host refusal to terminate raises a typed disposal failure")
       (test-assert (and (eq process *session-awake-process*)
                         (zerop (session-awake-test-process-waits process)))
                    "failed disposal retains handle and never blocks in wait")
       (setf (session-awake-test-platform-termination-mode *platform*) ':force)
       (session-awake-release)
       (test-assert (and (not (session-awake-test-process-alive-p process))
                         (= 1 (session-awake-test-process-waits process)))
                    "explicit release can retry failed disposal with forced termination"))))
  nil)


(-> test-session-awake-checkpoint-quiescence () null)
(defun test-session-awake-checkpoint-quiescence ()
  "Exercise checkpoint return, rollback and optout through the native quiescer."
  (session-awake-test--call-with-state
   (lambda ()
     (let ((application (allocate-instance (find-class 'application))))
       (setf (application-localgroup-session application) nil)
       (session-awake-acquire)
       (session-awake-monitor-start)
       (let ((process *session-awake-process*)
             (monitor *session-awake-monitor*))
         (test-assert
          (equal '("saved" 42)
                 (multiple-value-list
                  (application-call-with-localgroup-quiesced
                   application
                   (lambda ()
                     (test-assert
                      (and (null *session-awake-process*)
                           (null *session-awake-monitor*)
                           (not (thread-alive-p monitor))
                           (not (session-awake-test-process-alive-p process))
                           (= 1 (session-awake-test-process-waits process)))
                      "checkpoint executes after helper reap and monitor join")
                     (values "saved" 42)))))
          "quiescence preserves all save return values")
         (test-assert
          (and *session-awake-process* *session-awake-monitor*
               (not (eq process *session-awake-process*))
               (thread-alive-p *session-awake-monitor*))
          "normal save return reacquires helper and monitoring"))
       (test-assert
        (eq ':rolled-back
            (catch 'checkpoint-rollback
              (application-call-with-localgroup-quiesced
               application
               (lambda () (throw 'checkpoint-rollback ':rolled-back)))))
        "checkpoint rollback preserves the original nonlocal exit")
       (test-assert (and *session-awake-process* *session-awake-monitor*
                         (thread-alive-p *session-awake-monitor*))
                    "rollback reacquires ownership and monitoring")
       (session-awake-release)
       (let ((launches (session-awake-test-platform-launches *platform*)))
         (application-call-with-localgroup-quiesced application (lambda () t))
         (test-assert (= launches (session-awake-test-platform-launches *platform*))
                      "inactive checkpoint never acquires an inhibitor"))
       (session-awake-acquire)
       (application-call-with-localgroup-quiesced application (lambda () t))
       (test-assert (and *session-awake-process* (null *session-awake-monitor*))
                    "unmonitored acquisition resumes without a monitor"))))
  nil)
