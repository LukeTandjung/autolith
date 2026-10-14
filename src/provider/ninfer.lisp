(in-package #:autolith)

;;;; -- NInfer Chat Completions Provider --

(defparameter *ninfer-chat-completions-endpoint*
  "http://127.0.0.1:8080/v1/chat/completions"
  "The default cross-platform NInfer Chat Completions endpoint.")

(defparameter *ninfer-maximum-output-tokens* 32768
  "The maximum output token count accepted by NInfer.")

(defclass ninfer-credential-manager
    (credential-manager cl-rfc8628:static-credential-manager) ()
  (:documentation "In-memory request credentials for the shared retry protocol."))

(defclass ninfer-provider (openai-compatible-provider) ()
  (:documentation "Unauthenticated NInfer transport with retained assistant reasoning."))

(defmethod credential-manager-load ((manager ninfer-credential-manager))
  "Return a non-secret retry sentinel without reading or writing any file."
  (declare (ignore manager))
  ;; The shared OAuth record requires a pathname; this marker is never opened.
  (make-instance 'oauth-credentials
                 :access-token "ninfer-no-auth"
                 :refresh-token nil :id-token nil :account-id "ninfer"
                 :expires-at nil :source-path #P""))

(defmethod credential-manager-login-hint ((manager ninfer-credential-manager))
  "Explain that NInfer has no authentication flow."
  (declare (ignore manager))
  "NInfer requires no authentication")

(defmethod provider-authenticate ((provider ninfer-provider)
                                 &key stream open-browser-p)
  "Return the no-auth account policy without opening a credential store."
  (declare (ignore provider stream open-browser-p))
  "NInfer requires no authentication.")

(defmethod openai-compatible-provider-output-ceiling-field
    ((provider ninfer-provider))
  "Use NInfer's max_tokens output limit."
  (declare (ignore provider))
  "max_tokens")

(-> ninfer--thinking-options ((option string)) json-object)
(defun ninfer--thinking-options (effort)
  "Map EFFORT to NInfer template options, offering USE-VALUE for invalid effort."
  (loop
    (cond
      ((member effort '(nil "none" "minimal") :test #'equal)
       (return (json-object "enable_thinking" yason:false
                            "preserve_thinking" t "reasoning_effort" "low")))
      ((member effort '("low" "medium" "high" "xhigh") :test #'equal)
       (return (json-object "enable_thinking" t "preserve_thinking" t
                            "reasoning_effort"
                            (if (member effort '("high" "xhigh") :test #'equal)
                                "xhigh" effort))))
      (t
       (setf effort
             (restart-case
                 (error 'configuration-error
                        :message "NInfer effort must be none, minimal, low, medium, high, or xhigh.")
               (use-value (value)
                 :report "Supply a supported NInfer reasoning effort."
                 value)))))))

(-> ninfer--chat-messages (list) list)
(defun ninfer--chat-messages (items)
  "Attach round reasoning to assistant text and tool calls in ordered ITEMS."
  (let ((messages nil) (assistant nil) (reasoning nil) (calls nil))
    (labels ((flush-assistant ()
               (when (or assistant reasoning calls)
                 (let ((message (or assistant
                                    (json-object "role" "assistant" "content" ""))))
                   (when reasoning
                     (setf (gethash "reasoning_content" message) reasoning))
                   (when calls
                     (setf (gethash "tool_calls" message)
                           (coerce (nreverse calls) 'vector)))
                   (push message messages)))
               (setf assistant nil reasoning nil calls nil)))
      (dolist (item items)
        (cond
          ((clinker-transcript:chat-reasoning-item-p item)
           (flush-assistant)
           (setf reasoning (json-get item "content")))
          ((clinker-transcript:function-call-item-p item)
           (push (openai-compatible--chat-function-call-entry item) calls))
          ((and (json-object-p item)
                (json-string= (json-get item "type") "message")
                (json-string= (json-get item "role") "assistant"))
           (when assistant (flush-assistant))
           (setf assistant (openai-compatible--chat-message item)))
          (t
           (flush-assistant)
           (let ((message (openai-compatible--chat-input-item item)))
             (when message (push message messages))))))
      (flush-assistant)
      (nreverse messages))))

(defmethod provider-request-object
    ((provider ninfer-provider)
     (projection cl-llm-provider-api:wire-request) (tools vector)
     &key goal-context compaction-p)
  "Specialize shared request encoding with template options and retained reasoning."
  (declare (ignore goal-context compaction-p))
  (let* ((request (call-next-method))
         (options (cl-llm-provider-api:wire-request-options projection))
         (context (openai-compatible--chat-context-message
                   (cl-llm-provider-api:wire-request-suffix projection))))
    (setf (gethash "messages" request)
          (coerce
           (append
            (list (openai-compatible--chat-system-message
                   (cl-llm-provider-api:wire-request-prefix projection)))
            (ninfer--chat-messages
             (cl-llm-provider-api:wire-request-items projection))
            (when context (list context)))
           'vector)
          (gethash "chat_template_kwargs" request)
          (ninfer--thinking-options (getf options :reasoning-effort))
          (gethash "max_tokens" request)
          (min *ninfer-maximum-output-tokens*
               (or (getf options :maximum-output-tokens)
                   *ninfer-maximum-output-tokens*)))
    (remhash "max_completion_tokens" request)
    request))

(-> ninfer--request-headers (&key (:stream-p boolean)) list)
(defun ninfer--request-headers (&key (stream-p t))
  "Return ordinary HTTP headers without exposing request-scope credentials."
  (append (when stream-p (list (cons "Content-Type" "application/json")))
          (list (cons "Accept" (if stream-p "text/event-stream" "application/json"))
                (cons "User-Agent" (provider-user-agent)))))

(defmethod provider-open-response-stream
    ((provider ninfer-provider) (request hash-table)
     &key credentials conversation)
  "Open the shared SSE transport without an Authorization header."
  (declare (ignore credentials conversation))
  (provider-post-event-stream
   (config :provider-endpoint (provider-configuration provider))
   (json-encode-utf8 request) :headers (ninfer--request-headers)))

(-> ninfer-provider-create
    (configuration &key (:reasoning-summaries-p boolean)) ninfer-provider)
(defun ninfer-provider-create (configuration &key reasoning-summaries-p)
  "Create NInfer using the configured endpoint and the shared provider lifecycle."
  (declare (ignore reasoning-summaries-p))
  (make-instance 'ninfer-provider
                 :configuration configuration
                 :credential-manager (make-instance 'ninfer-credential-manager)
                 :session-id (make-identifier)
                 :display-name "ninfer" :family ':ninfer
                 :reasoning-parameter nil :stream-usage-p t))

(-> ninfer-models-endpoint (&optional configuration) non-empty-string)
(defun ninfer-models-endpoint (&optional configuration)
  "Derive the catalog URL from NInfer's registered or selected endpoint."
  (let* ((registration (provider-registration-find "ninfer"))
         (endpoint (if (and configuration
                            (eq (model-family (config :model configuration)) ':ninfer))
                       (config :provider-endpoint configuration)
                       (or (and registration
                                (provider-registration-endpoint registration))
                           *ninfer-chat-completions-endpoint*)))
         (suffix "/chat/completions"))
    (unless (uiop:string-suffix-p endpoint suffix)
      (error 'configuration-error
             :message "NInfer endpoint must end in /chat/completions for model discovery."))
    (concatenate 'string (subseq endpoint 0 (- (length endpoint) (length suffix)))
                 "/models")))

(defmethod openai-compatible-model-discovery-headers
    ((manager ninfer-credential-manager) (credentials oauth-credentials) custom)
  "Discover NInfer models without a credential or Authorization header."
  (declare (ignore manager credentials custom))
  (ninfer--request-headers :stream-p nil))

(-> ninfer--fetch-models (configuration) list)
(defun ninfer--fetch-models (configuration)
  "Discover served model identifiers with the shared HTTP and catalog decoder."
  (openai-compatible--fetch-models
   configuration :provider-name "ninfer"
   :endpoint (ninfer-models-endpoint configuration)
   :credential-manager (make-instance 'ninfer-credential-manager)))

(defmethod provider-family-create
    ((family (eql ':ninfer)) (configuration configuration)
     &key reasoning-summaries-p)
  "Create NInfer through the ordinary provider-family protocol."
  (ninfer-provider-create configuration :reasoning-summaries-p reasoning-summaries-p))
