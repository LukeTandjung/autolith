(in-package #:autolith)

;;;; -- Session Sleep Inhibition --

(define-condition session-awake-unavailable (autolith-error)
  ((backend
    :initarg :backend
    :reader session-awake-unavailable-backend
    :documentation "The host backend which could not hold inhibition.")
   (cause
    :initarg :cause
    :initform nil
    :reader session-awake-unavailable-cause
    :documentation "The underlying host failure, when available."))
  (:documentation "The host refused or lacks the requested sleep inhibitor."))

(defvar *session-awake-process* nil
  "The one sleep-inhibitor helper owned by this Lisp process.")

(defvar *session-awake-owner-pid* nil
  "PID for which the helper was acquired; restored heaps reacquire it.")

(defvar *session-awake-backend* nil
  "Backend of the owned helper, or :unavailable after an explicit optout.")

(defvar *session-awake-lock* (make-lock "session sleep inhibitor")
  "Serialize acquisition, monitoring and release.")

(defvar *session-awake-wakeup* (make-condition-variable)
  "Wake the monitor promptly during release.")

(defvar *session-awake-monitor* nil
  "The owned helper-liveness monitor thread.")

(defvar *session-awake-monitor-token* nil
  "Identity token cancelling the current monitor loop.")

(defvar *session-awake-failure* nil
  "Latest monitor failure; explicit acquisition clears it after success.")

(defparameter *session-awake-check-seconds* 5
  "Seconds between helper liveness checks.")

(-> session-awake--after-restore () null)
(defun session-awake--after-restore ()
  "Forget inherited OS handles before restored application threads start.

Do not acquire saved locks or inspect saved handles, even if a PID is reused."
  (setf *session-awake-process* nil
        *session-awake-owner-pid* nil
        *session-awake-backend* nil
        *session-awake-monitor* nil
        *session-awake-monitor-token* nil
        *session-awake-failure* nil
        *session-awake-lock* (make-lock "session sleep inhibitor")
        *session-awake-wakeup* (make-condition-variable))
  nil)

(-> session-awake--restore-hook-install () null)
(defun session-awake--restore-hook-install ()
  "Register restore invalidation before this process owns external handles."
  (pushnew 'session-awake--after-restore sb-ext:*init-hooks*)
  nil)

(-> session-awake--launch (keyword list) t)
(defun session-awake--launch (backend command)
  "Translate failures from the host process-launch boundary."
  (handler-case
      (platform-launch-sleep-inhibitor *platform* command)
    (error (condition)
      (error 'session-awake-unavailable :backend backend :cause condition
             :message "Could not start the sleep-inhibitor helper."))))

(-> session-awake--command (keyword (integer 1)) list)
(defun session-awake--command (backend pid)
  "Translate unavailable host capabilities before launching a helper."
  (handler-case
      (platform-sleep-inhibitor-command *platform* pid)
    (platform-capability-unavailable (condition)
      (error 'session-awake-unavailable :backend backend :cause condition
             :message "No sleep-inhibition backend is available for this host."))))

(-> session-awake--await-exit (t (real 0)) boolean)
(defun session-awake--await-exit (process seconds)
  "Poll for bounded time; reap only after confirmed exit."
  (let ((deadline (+ (get-internal-real-time)
                     (ceiling (* seconds internal-time-units-per-second)))))
    (loop
      (unless (platform-process-object-alive-p *platform* process)
        (return t))
      (when (>= (get-internal-real-time) deadline)
        (return nil))
      (sleep 0.02))))

(-> session-awake--dispose (t) null)
(defun session-awake--dispose (process)
  "Stop an owned helper with bounded TERM/KILL waits, then reap its exit.

The caller retains its handle on failure so explicit release can retry."
  (when (platform-process-object-alive-p *platform* process)
    (platform-terminate-process-object *platform* process)
    (unless (session-awake--await-exit process 1)
      (platform-terminate-process-object *platform* process :force t)
      (unless (session-awake--await-exit process 1)
        (error 'session-awake-unavailable :backend *session-awake-backend*
               :message "The sleep-inhibitor helper did not exit after forced termination."))))
  (platform-wait-process *platform* process)
  nil)

(-> session-awake--acquire-locked ((integer 1)) keyword)
(defun session-awake--acquire-locked (pid)
  "Acquire with the lifecycle lock held; never inspect an inherited handle."
  (unless (eql *session-awake-owner-pid* pid)
    (setf *session-awake-monitor* nil
          *session-awake-monitor-token* nil))
  (when (and (eql *session-awake-owner-pid* pid) *session-awake-process*)
    (when (platform-process-object-alive-p *platform* *session-awake-process*)
      (setf *session-awake-failure* nil)
      (return-from session-awake--acquire-locked *session-awake-backend*))
    (session-awake--dispose *session-awake-process*))
  (setf *session-awake-process* nil
        *session-awake-owner-pid* pid
        *session-awake-backend* nil)
  (let* ((backend (platform-sleep-inhibitor-backend *platform*))
         (command (session-awake--command backend pid))
         (process (session-awake--launch backend command))
         (installed-p nil))
    (unwind-protect
         (progn
           ;; Detect immediate command/policy failures, not merely a successful fork.
           (sleep 0.15)
           (unless (platform-process-object-alive-p *platform* process)
             (error 'session-awake-unavailable :backend backend
                    :message "The sleep-inhibitor helper exited during startup."))
           (setf *session-awake-process* process
                 *session-awake-backend* backend
                 *session-awake-failure* nil
                 installed-p t)
           backend)
      (unless installed-p
        (handler-case
            (session-awake--dispose process)
          (error (condition)
            (setf *session-awake-process* process
                  *session-awake-backend* backend)
            (error condition)))))))

(-> session-awake-acquire () (option keyword))
(defun session-awake-acquire ()
  "Acquire idempotently, including after heap restoration.

Use CONTINUE-WITHOUT-INHIBITOR on a host without inhibition support.
Each helper watches its owner process, including on abrupt Lisp exit."
  (session-awake--restore-hook-install)
  (restart-case
      (with-lock-held (*session-awake-lock*)
        (session-awake--acquire-locked (current-process-id)))
    (continue-without-inhibitor ()
      :report "Start this session without preventing host sleep."
      (with-lock-held (*session-awake-lock*)
        (setf *session-awake-backend* ':unavailable))
      nil)))

(-> session-awake--monitor-check ((integer 1) (integer 0))
    (values boolean (integer 0)))
(defun session-awake--monitor-check (pid retries-left)
  "Check with the lock held; return continue-p and the bounded retry balance."
  (unless (eql pid *session-awake-owner-pid*)
    (return-from session-awake--monitor-check (values nil retries-left)))
  (handler-case
      (cond
        ((and *session-awake-process*
              (platform-process-object-alive-p *platform* *session-awake-process*))
         (values t retries-left))
        ((plusp retries-left)
         (session-awake--acquire-locked pid)
         (values t (1- retries-left)))
        (t
         (error 'session-awake-unavailable :backend *session-awake-backend*
                :message "The replacement sleep-inhibitor helper exited; automatic retries stopped.")))
    (error (condition)
      (setf *session-awake-failure*
            (if (typep condition 'session-awake-unavailable)
                condition
                (make-condition 'session-awake-unavailable
                                :backend *session-awake-backend* :cause condition
                                :message "Sleep-inhibitor monitoring failed.")))
      (values nil 0))))

(-> session-awake-monitor-start () t)
(defun session-awake-monitor-start ()
  "Watch the helper with one automatic replacement per explicit start.

After another failure the monitor stops and records SESSION-AWAKE-FAILURE.
Call SESSION-AWAKE-ACQUIRE explicitly to retry, then restart monitoring."
  (with-lock-held (*session-awake-lock*)
    (let ((pid (current-process-id)))
      (unless (and (eql pid *session-awake-owner-pid*) *session-awake-process*)
        (return-from session-awake-monitor-start nil))
      (when (and *session-awake-monitor-token* *session-awake-monitor*
                 (thread-alive-p *session-awake-monitor*))
        (return-from session-awake-monitor-start *session-awake-monitor*))
      (let ((token (list pid)))
        (setf *session-awake-monitor-token* token
              *session-awake-monitor*
              (make-thread
               (lambda ()
                 (let ((retries-left 1))
                   (with-lock-held (*session-awake-lock*)
                     (loop while (eq token *session-awake-monitor-token*) do
                       (condition-wait *session-awake-wakeup* *session-awake-lock*
                                       :timeout *session-awake-check-seconds*)
                       (unless (eq token *session-awake-monitor-token*)
                         (return))
                       (multiple-value-bind (continue-p remaining)
                           (session-awake--monitor-check pid retries-left)
                         (setf retries-left remaining)
                         (unless continue-p
                           (setf *session-awake-monitor-token* nil)
                           (return)))))))
               :name "session sleep-inhibitor monitor"))))))

(-> session-awake-release () null)
(defun session-awake-release ()
  "Stop monitoring and release only this process's inhibitor.

Inherited handles in saved heaps are forgotten, never signalled or waited on."
  (let ((monitor nil))
    (unwind-protect
         (with-lock-held (*session-awake-lock*)
           (when (eql *session-awake-owner-pid* (current-process-id))
             (setf monitor *session-awake-monitor*
                   *session-awake-monitor-token* nil)
             (condition-notify *session-awake-wakeup*)
             (when *session-awake-process*
               (session-awake--dispose *session-awake-process*)))
           (setf *session-awake-process* nil
                 *session-awake-owner-pid* nil
                 *session-awake-backend* nil
                 *session-awake-monitor* nil
                 *session-awake-monitor-token* nil
                 *session-awake-failure* nil))
      ;; Join outside the lock needed by the monitor's exit path.
      (when monitor
        (join-thread monitor))))
  nil)

(-> session-awake-call-quiesced (function) t)
(defun session-awake-call-quiesced (function)
  "Call FUNCTION without helper handles or monitor threads, then restore ownership.

Only an inhibitor owned by this process is reacquired on return or unwind.
Preserve whether monitoring was active; inactive and opted-out sessions stay idle."
  (let ((owned-p nil)
        (monitored-p nil))
    (with-lock-held (*session-awake-lock*)
      (setf owned-p (and (eql *session-awake-owner-pid* (current-process-id))
                        (not (null *session-awake-process*)))
            monitored-p (and owned-p *session-awake-monitor-token*
                             *session-awake-monitor*
                             (thread-alive-p *session-awake-monitor*))))
    (unwind-protect
         (progn
           (session-awake-release)
           (funcall function))
      (when (and owned-p (session-awake-acquire) monitored-p)
        (session-awake-monitor-start)))))

(-> session-longevity-install
    (configuration &key (:interactive-p boolean) (:pristine-p boolean)) null)
(defun session-longevity-install (configuration &key interactive-p pristine-p)
  "Apply session sleep policy after user configuration on interactive startup.

Headless, test and pristine startup pass INTERACTIVE-P NIL or PRISTINE-P T."
  (when (and interactive-p (not pristine-p))
    (session-awake--restore-hook-install)
    (pushnew 'session-awake-release sb-ext:*exit-hooks*)
    (if (config :prevent-session-sleep-p configuration)
        (when (session-awake-acquire)
          (session-awake-monitor-start))
        (session-awake-release)))
  nil)
