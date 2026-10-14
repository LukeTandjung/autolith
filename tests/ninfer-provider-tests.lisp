(in-package #:autolith)

;;;; -- NInfer Provider Tests --

(defclass ninfer-provider-test (ninfer-provider)
  ((fixture :initarg :fixture
            :reader ninfer-provider-test-fixture
            :type string
            :documentation "One synthetic streaming Chat Completions response."))
  (:documentation "NInfer stream fixture exercising the inherited SSE parser."))

(defmethod provider-open-response-stream
    ((provider ninfer-provider-test) (request hash-table)
     &key credentials conversation)
  "Read the synthetic SSE response without network access."
  (declare (ignore request credentials conversation))
  (values (make-string-input-stream (ninfer-provider-test-fixture provider)) 200 nil))

(-> ninfer-provider-test--stream (boolean) string)
(defun ninfer-provider-test--stream (tool-p)
  "Return reasoning followed by either assistant text or a function call."
  (with-output-to-string (stream)
    (flet ((event (object)
             (format stream "data: ~A~%~%" (json-encode object))))
      (event (json-object "id" "ninfer-test-response"
                          "choices" (vector (json-object
                                             "index" 0 "delta"
                                             (json-object "role" "assistant")))))
      (event (json-object "id" "ninfer-test-response"
                          "choices" (vector (json-object
                                             "index" 0 "delta"
                                             (json-object "reasoning_content"
                                                          "Check the source.")))))
      (event (json-object
              "id" "ninfer-test-response"
              "choices" (vector
                         (json-object
                          "index" 0 "delta"
                          (if tool-p
                              (json-object
                               "tool_calls"
                               (vector (json-object
                                        "index" 0 "id" "ninfer-test-call"
                                        "type" "function"
                                        "function"
                                        (json-object "name"
                                                     (openai-compatible--wire-tool-name "resource" "read")
                                                     "arguments" "{\"uri\":\"workspace:.\"}"))))
                              (json-object "content" "Ready."))))))
      (event (json-object "id" "ninfer-test-response"
                          "choices" (vector (json-object
                                             "index" 0 "delta" (json-object)
                                             "finish_reason"
                                             (if tool-p "tool_calls" "stop")))))
      (event (json-object "id" "ninfer-test-response" "choices" #()
                          "usage" (json-object "prompt_tokens" 10
                                               "completion_tokens" 5
                                               "total_tokens" 15)))
      (format stream "data: [DONE]~%~%"))))

(-> ninfer-provider-test--configuration () configuration)
(defun ninfer-provider-test--configuration ()
  "Register a synthetic model and select its independently configured endpoint."
  (register-provider "ninfer-test" :family ':ninfer
                     :protocol ':chat-completions
                     :models '((:name "ninfer-test/model" :context-window 32000
                                :reasoning-efforts ("none" "minimal" "low" "medium" "high" "xhigh")))
                     :endpoint "http://ninfer.invalid/v1/chat/completions"
                     :factory #'ninfer-provider-create :source ':runtime)
  (configuration-copy (test-configuration) :model "ninfer-test/model"))

(-> test-ninfer-provider-request () null)
(defun test-ninfer-provider-request ()
  "Test effort mapping, token limits, and unauthenticated streaming transport."
  (let* ((snapshot (provider--registry-snapshot))
         (configuration (ninfer-provider-test--configuration))
         (root (test-configuration-root configuration))
         (provider (provider-create configuration)))
    (unwind-protect
         (progn
           (dolist (case '((nil nil "low") ("none" nil "low")
                           ("minimal" nil "low") ("low" t "low")
                           ("medium" t "medium") ("high" t "xhigh")
                           ("xhigh" t "xhigh")))
             (dolist (limit '(nil 100 65536))
               (let* ((projection
                        (make-instance 'cl-llm-provider-api:wire-request
                                       :model "test-model" :prefix '("instructions")
                                       :suffix nil :items nil
                                       :options (list :reasoning-effort (first case)
                                                      :maximum-output-tokens limit)))
                      (request (provider-request-object provider projection #()))
                      (options (json-get request "chat_template_kwargs")))
                 (test-assert (= (json-get request "max_tokens")
                                 (min 32768 (or limit 32768)))
                              "NInfer bounds output tokens by its ceiling")
                 (test-assert (null (json-get request "max_completion_tokens"))
                              "NInfer uses the legacy max_tokens field")
                 (test-assert (eq (json-get options "enable_thinking")
                                  (if (second case) t yason:false))
                              "effort controls template thinking")
                 (test-assert (eq t (json-get options "preserve_thinking"))
                              "template preserves assistant thinking")
                 (test-assert (equal (third case) (json-get options "reasoning_effort"))
                              "effort maps to NInfer's finite vocabulary"))))
           (test-assert
            (handler-case (progn (ninfer--thinking-options "invalid") nil)
              (configuration-error () t))
            "unsupported effort signals a typed configuration error")
           (handler-bind ((configuration-error
                            (lambda (condition)
                              (declare (ignore condition))
                              (invoke-restart 'use-value "low"))))
             (test-assert (equal "low" (json-get (ninfer--thinking-options "invalid")
                                                  "reasoning_effort"))
                          "effort can be corrected with USE-VALUE"))
           (let ((original (symbol-function 'provider-post-event-stream))
                 (sent nil))
             (unwind-protect
                  (progn
                    (setf (symbol-function 'provider-post-event-stream)
                          (lambda (endpoint bytes &key headers)
                            (declare (ignore bytes))
                            (setf sent (list endpoint headers))
                            (values (make-string-input-stream "") nil)))
                    (close (provider-open-response-stream
                            provider (json-object)
                            :credentials
                            (credential-manager-load (provider-credential-manager provider))))
                    (test-assert (equal (first sent)
                                        "http://ninfer.invalid/v1/chat/completions")
                                 "stream requests use the configured endpoint")
                    (test-assert (null (assoc "Authorization" (second sent)
                                              :test #'string-equal))
                                 "the credential sentinel is never sent")
                    (test-assert (equal "text/event-stream"
                                        (rest (assoc "Accept" (second sent)
                                                     :test #'string-equal)))
                                 "NInfer requests streaming responses"))
               (setf (symbol-function 'provider-post-event-stream) original)))
           (test-assert (not (probe-file (configuration-api-keys-path configuration)))
                        "NInfer does not create an API-key file"))
      (provider--registry-restore snapshot)
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-ninfer-provider-stream () null)
(defun test-ninfer-provider-stream ()
  "Test inherited SSE parsing and reasoning history across two assistant rounds."
  (let* ((snapshot (provider--registry-snapshot))
         (configuration (ninfer-provider-test--configuration))
         (root (test-configuration-root configuration)))
    (unwind-protect
         (let ((conversation (conversation-create configuration))
               (round-items nil))
           (dolist (tool-p '(nil t))
             (let* ((provider
                      (make-instance 'ninfer-provider-test
                                     :configuration configuration
                                     :credential-manager (make-instance 'ninfer-credential-manager)
                                     :session-id "ninfer-test" :display-name "ninfer"
                                     :family ':ninfer
                                     :fixture (ninfer-provider-test--stream tool-p)))
                    (result (provider-stream-turn
                             provider conversation :tool-namespaces #()
                             :event-callback (lambda (event) (declare (ignore event)))))
                    (items (coerce (provider-result-output-items result) 'list))
                    (messages (ninfer--chat-messages items))
                    (assistant (first messages)))
               (test-assert (equal "Check the source."
                                   (json-get assistant "reasoning_content"))
                            "SSE reasoning is retained on assistant history")
               (if tool-p
                   (progn
                     (test-assert (= 1 (length (provider-result-tool-calls result)))
                                  "inherited SSE parsing yields the tool call")
                     (test-assert (= 1 (length (json-get assistant "tool_calls")))
                                  "reasoning accompanies the assistant tool call"))
                   (let ((parts (json-get assistant "content")))
                     (test-assert (= 1 (length parts)) "assistant text is one content part")
                     (test-assert (equal "Ready." (json-get (aref parts 0) "text"))
                                  "reasoning accompanies assistant text")))
               (setf round-items (append round-items items))))
           (let ((messages (ninfer--chat-messages round-items)))
             (test-assert (= 2 (length messages)) "distinct reasoning rounds remain distinct")
             (test-assert (every (lambda (message)
                                  (equal "Check the source."
                                         (json-get message "reasoning_content")))
                                messages)
                           "both rounds retain their reasoning"))
            (let ((provider (provider-create configuration))
                  (attempts 0)
                  (*bounded-retry-sleep-function*
                    (lambda (delay) (declare (ignore delay)))))
              (test-call-with-function-replacements
               (list
                (list 'provider-post-event-stream
                      (lambda (url body &key headers)
                        (declare (ignore url body))
                        (test-assert
                         (null (assoc "Authorization" headers :test #'string-equal))
                         "every retry uses the unauthenticated transport")
                        (incf attempts)
                        (if (= attempts 1)
                            (values (make-string-input-stream
                                     "{\"error\":{\"message\":\"retry\"}}") 503 nil)
                            (values (make-string-input-stream
                                     (ninfer-provider-test--stream nil)) 200 nil)))))
               (lambda ()
                 (let ((result
                         (provider-stream-turn
                          provider conversation :tool-namespaces #()
                          :event-callback (lambda (event) (declare (ignore event))))))
                   (test-assert (and (= attempts 2)
                                     (equal "Ready." (provider-result-assistant-text result)))
                                "shared transient retries recover a NInfer streaming request")
                   (test-assert (not (probe-file (configuration-api-keys-path configuration)))
                                "retrying NInfer needs no credential file"))))))
      (provider--registry-restore snapshot)
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)


(-> test-ninfer-provider-discovery () null)
(defun test-ninfer-provider-discovery ()
  "Test unauthenticated catalog discovery and its shared typed failure paths."
  (let* ((snapshot (provider--registry-snapshot))
         (configuration (ninfer-provider-test--configuration))
         (root (test-configuration-root configuration))
         (original-get (symbol-function 'dexador:get))
         (status 200)
         (body "{\"data\":[{\"id\":\"ninfer-served/model\",\"context_length\":65536}]}")
         (sent nil))
    (unwind-protect
         (progn
           (setf (symbol-function 'dexador:get)
                 (lambda (url &rest arguments)
                   (setf sent (list url (getf arguments :headers)))
                   (values body status nil nil)))
           (let ((models (ninfer--fetch-models configuration)))
             (test-assert (equal "ninfer-served/model" (model-spec-name (first models)))
                          "shared catalog decoding returns the served model")
             (test-assert (equal "http://ninfer.invalid/v1/models" (first sent))
                          "discovery derives its URL from the selected endpoint")
             (test-assert (null (assoc "Authorization" (second sent) :test #'string-equal))
                          "catalog requests do not send authorization")
             (test-assert (equal "application/json"
                                 (rest (assoc "Accept" (second sent) :test #'string-equal)))
                          "catalog requests accept JSON"))
           (setf status 503)
           (test-assert
            (handler-case (progn (ninfer--fetch-models configuration) nil)
              (configuration-error () t))
            "catalog HTTP failures use the shared typed status path")
           (setf status 401)
           (test-assert
            (handler-case (progn (ninfer--fetch-models configuration) nil)
              (authentication-error () t))
            "a server requiring authentication reports its status explicitly")
           (setf status 200 body "not-json")
           (test-assert
            (handler-case (progn (ninfer--fetch-models configuration) nil)
              (configuration-error () t))
            "malformed catalogs use the shared decoder failure path")
           (test-assert (not (probe-file (configuration-api-keys-path configuration)))
                        "catalog discovery never creates credentials"))
      (setf (symbol-function 'dexador:get) original-get)
      (provider--registry-restore snapshot)
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)
