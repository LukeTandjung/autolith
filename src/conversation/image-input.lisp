(in-package #:autolith)

;;;; -- User Input --

(defgeneric user-message-input-text (input)
  (:documentation "Return the editable text carried by user INPUT."))

(defgeneric user-message-input-image-pathnames (input)
  (:documentation "Return the absolute local image pathnames attached to user INPUT."))

(defgeneric user-message-input-file-contexts (input)
  (:documentation "Return the selected workspace file snapshots attached to user INPUT."))

(defparameter *workspace-file-context-maximum-file-bytes* (* 128 1024)
  "The maximum exact UTF-8 content snapshot for one workspace file.")

(defparameter *workspace-file-context-maximum-combined-bytes* (* 256 1024)
  "The maximum combined workspace file content in one user submission.")

(defclass workspace-file-context ()
  ((path
    :initarg :path
    :reader workspace-file-context-path
    :type non-empty-string
    :documentation "The workspace-relative path shown in the editor.")
   (root
    :initarg :root
    :reader workspace-file-context-root
    :type pathname
    :documentation "The canonical workspace root at selection time.")
   (pathname
    :initarg :pathname
    :reader workspace-file-context-pathname
    :type pathname
    :documentation "The canonical selected file pathname at selection time.")
   (content
    :initarg :content
    :reader workspace-file-context-content
    :type string
    :documentation "The exact bounded UTF-8 content snapshot.")
   (byte-count
    :initarg :byte-count
    :reader workspace-file-context-byte-count
    :type (integer 0)
    :documentation "The UTF-8 size of CONTENT."))
  (:documentation "One immutable workspace file context snapshot."))

(-> workspace-file-context--utf-8-bytes (string) (integer 0))
(defun workspace-file-context--utf-8-bytes (content)
  "Return the number of UTF-8 bytes in CONTENT."
  (length (sb-ext:string-to-octets content :external-format ':utf-8)))

(-> workspace-file-context-create
    (&key (:path string) (:root pathname) (:pathname pathname)
          (:content string))
    workspace-file-context)
(defun workspace-file-context-create (&key path root pathname content)
  "Create one validated immutable workspace file context snapshot."
  (unless (and (non-empty-string-p path)
               (not (uiop:absolute-pathname-p (uiop:parse-native-namestring path)))
               (not (find-if (lambda (character)
                               (let ((code (char-code character)))
                                 (or (< code 32) (= code 127))))
                             path)))
    (error 'configuration-error
           :message "A workspace file context requires a safe relative path."))
  (unless (and (uiop:absolute-pathname-p root)
               (uiop:absolute-pathname-p pathname)
               (uiop:subpathp pathname root))
    (error 'configuration-error
           :message "A workspace file context pathname must be inside its root."))
  (let ((byte-count (workspace-file-context--utf-8-bytes content)))
    (when (> byte-count *workspace-file-context-maximum-file-bytes*)
      (error 'configuration-error
             :message
             (format nil "Workspace file context ~A exceeds the ~:D-byte limit."
                     path *workspace-file-context-maximum-file-bytes*)))
    (make-instance 'workspace-file-context
                   :path (copy-seq path)
                   :root root
                   :pathname pathname
                   :content (copy-seq content)
                   :byte-count byte-count)))

(-> workspace-file-context-snapshot (pathname string) workspace-file-context)
(defun workspace-file-context-snapshot (workspace-root relative-path)
  "Read RELATIVE-PATH below WORKSPACE-ROOT as a bounded stable UTF-8 snapshot."
  (let* ((root (workspace-tool--canonical-path workspace-root))
         (relative (uiop:parse-native-namestring relative-path)))
    (when (uiop:absolute-pathname-p relative)
      (error 'tool-error
             :message "Inline file context requires a workspace-relative path."
             :tool-name "@file"))
    (let* ((candidate (merge-pathnames relative root))
           (path (workspace-tool--canonical-path candidate)))
      (unless (uiop:subpathp path root)
        (error 'tool-error
               :message (format nil "Path ~A is outside the workspace." relative-path)
               :tool-name "@file"))
      (workspace-file-context-create
       :path relative-path
       :root root
       :pathname path
       :content
       (file--read-bounded-utf-8
        candidate
        :maximum-bytes *workspace-file-context-maximum-file-bytes*
        :tool-name "@file"
        :description "Workspace file"
        :validation-function
        (lambda ()
          (unless (uiop:subpathp (workspace-tool--canonical-path candidate)
                                 root)
            (error 'tool-error
                   :message (format nil "Path ~A is outside the workspace."
                                    relative-path)
                   :tool-name "@file"))))))))

(defclass user-message-input ()
  ((text
    :initarg :text
    :reader user-message-input-text
    :type string
    :documentation "The editable user text, including visible attachment labels.")
   (image-pathnames
    :initarg :image-pathnames
    :initform nil
    :reader user-message-input-image-pathnames
    :type list
    :documentation "Absolute local image pathnames attached to this submission.")
   (file-contexts
    :initarg :file-contexts
    :initform nil
    :reader user-message-input-file-contexts
    :type list
    :documentation "Immutable selected workspace file content snapshots."))
  (:documentation "One user submission containing text, images, and file context."))

(-> user-message-input-text ((or string user-message-input)) string)
(-> user-message-input-image-pathnames ((or string user-message-input)) list)
(-> user-message-input-file-contexts ((or string user-message-input)) list)

(defmethod user-message-input-text ((input string))
  input)

(defmethod user-message-input-image-pathnames ((input string))
  (declare (ignore input)))

(defmethod user-message-input-file-contexts ((input string))
  (declare (ignore input)))

(-> user-message-input-create
    (&key (:text string) (:image-pathnames list) (:file-contexts list))
    user-message-input)
(defun user-message-input-create (&key (text "") image-pathnames file-contexts)
  "Create one validated user submission from text, images, and file context."
  (unless (or (non-empty-string-p text) image-pathnames file-contexts)
    (error 'configuration-error
           :message "A user submission requires text, an image, or file context."))
  (unless (every #'pathnamep image-pathnames)
    (error 'configuration-error
           :message "Every attached image must have an absolute pathname."))
  (unless (every #'uiop:absolute-pathname-p image-pathnames)
    (error 'configuration-error
           :message "Every attached image pathname must be absolute."))
  (unless (every (lambda (context)
                   (typep context 'workspace-file-context))
                 file-contexts)
    (error 'configuration-error
           :message "Every attached file context must be a workspace snapshot."))
  (when (> (reduce #'+ file-contexts
                   :key #'workspace-file-context-byte-count
                   :initial-value 0)
           *workspace-file-context-maximum-combined-bytes*)
    (error 'configuration-error
           :message
           (format nil "Workspace file context exceeds the ~:D-byte combined limit."
                   *workspace-file-context-maximum-combined-bytes*)))
  (make-instance 'user-message-input
                 :text text
                 :image-pathnames (copy-list image-pathnames)
                 :file-contexts (copy-list file-contexts)))

(-> user-message-input-copy ((or string user-message-input))
    (or string user-message-input))
(defgeneric user-message-input-copy (input)
  (:documentation "Return a detached copy of user INPUT."))

(defmethod user-message-input-copy ((input string))
  (copy-seq input))

(defmethod user-message-input-copy ((input user-message-input))
  (make-instance 'user-message-input
                 :text (copy-seq (user-message-input-text input))
                 :image-pathnames
                 (copy-list (user-message-input-image-pathnames input))
                 :file-contexts
                 (copy-list (user-message-input-file-contexts input))))


;;;; -- Prepared Attachments --

(defparameter *image-input-maximum-source-bytes* (* 1024 1024 1024)
  "The Codex-compatible sanity limit for one source image.")

(defparameter *image-input-maximum-dimension* 2048
  "The maximum high-detail prompt-image width or height.")

(defparameter *image-input-patch-size* 32
  "The provider image-token patch width and height.")

(defparameter *image-input-maximum-patches* 2500
  "The maximum high-detail prompt-image patch count.")

(defclass image-attachment ()
  ((identifier
    :initarg :identifier
    :reader image-attachment-identifier
    :type non-empty-string
    :documentation "The stable identifier of this conversation artifact.")
   (pathname
    :initarg :pathname
    :reader image-attachment-pathname
    :type pathname
    :documentation "The private prepared image artifact pathname.")
   (source-name
    :initarg :source-name
    :reader image-attachment-source-name
    :type non-empty-string
    :documentation "The original absolute pathname shown to the model.")
   (mime-type
    :initarg :mime-type
    :reader image-attachment-mime-type
    :type non-empty-string
    :documentation "The media type of the prepared artifact.")
   (width
    :initarg :width
    :reader image-attachment-width
    :type (integer 1)
    :documentation "The prepared image width in pixels.")
   (height
    :initarg :height
    :reader image-attachment-height
    :type (integer 1)
    :documentation "The prepared image height in pixels."))
  (:documentation "A validated provider-ready image stored outside conversation text."))

(setf yolokuva:*image-error-class* 'image-input-error)

(-> image-input--error (pathname keyword string &optional t) null)
(defun image-input--error (pathname stage message &optional cause)
  "Signal a structured image failure for PATHNAME at STAGE."
  (error 'image-input-error
         :message message
         :pathname pathname
         :stage stage
         :cause cause))

(-> image-input--limits () list)
(defun image-input--limits ()
  "Return the Codex high-detail size limits as yolokuva keyword arguments."
  (list :maximum-dimension *image-input-maximum-dimension*
        :patch-size *image-input-patch-size*
        :maximum-patches *image-input-maximum-patches*))

(-> image-input--absolute (pathname) pathname)
(defun image-input--absolute (pathname)
  "Return PATHNAME resolved through the platform, or signal a recognition failure."
  (handler-case
      (platform-truename *platform* pathname)
    (error (condition)
      (image-input--error
       pathname ':recognition
       (format nil "Image ~A does not exist or cannot be read." pathname)
       condition))))

(-> image-input--inspect
    (pathname)
    (values keyword integer integer))
(defun image-input--inspect (pathname)
  "Identify PATHNAME, returning its image format and pixel dimensions."
  (yolokuva:image-inspect (image-input--absolute pathname)
                          :maximum-octets *image-input-maximum-source-bytes*))

(-> image-input-prepare (pathname pathname) image-attachment)
(defun image-input-prepare (source artifact-root)
  "Validate SOURCE and publish its prompt-ready form privately beneath ARTIFACT-ROOT.

The prepared image stays within the Codex high-detail limits: a fitting PNG,
JPEG, or WebP image is copied, anything else becomes a resized PNG."
  (let ((absolute (image-input--absolute source)))
    (multiple-value-bind (format width height) (image-input--inspect absolute)
      (multiple-value-bind (output-format target-width target-height)
          (apply #'yolokuva:image-preparation format width height
                 :pathname absolute (image-input--limits))
        (let* ((identifier (make-identifier))
               (target (merge-pathnames
                        (make-pathname :name identifier
                                       :type (ecase output-format
                                               (:png "png")
                                               (:jpeg "jpg")
                                               (:webp "webp")))
                        artifact-root)))
          (ensure-directories-exist target)
          (platform-make-private *platform* artifact-root)
          (handler-case
              (progn
                (publish-pathname
                 target
                 (lambda (temporary)
                   (yolokuva:image-write-prepared absolute temporary
                                                  :source-format format
                                                  :output-format output-format
                                                  :width target-width
                                                  :height target-height)))
                (platform-make-private *platform* target :read-only-p t))
            (image-input-error (condition)
              (error condition))
            (error (condition)
              (image-input--error
               absolute ':persistence
               (format nil "Image ~A could not be stored: ~A" absolute condition)
               condition)))
          (make-instance 'image-attachment
                         :identifier identifier
                         :pathname target
                         :source-name (namestring absolute)
                         :mime-type (yolokuva:image-mime-type output-format)
                         :width target-width
                         :height target-height))))))


;;;; -- Paste Recognition --

(-> image-input-normalize-pasted-path (string) (option pathname))
(defun image-input-normalize-pasted-path (text)
  "Normalize one pasted local path or file URL into an absolute existing pathname."
  (let ((path (clinedi:pasted-path text)))
    (when (non-empty-string-p path)
      (let ((pathname (uiop:ensure-pathname (platform-pathname path)
                                            :defaults (uiop:getcwd)
                                            :ensure-absolute t
                                            :want-non-wild t)))
        (and (uiop:file-exists-p pathname) (platform-truename *platform* pathname))))))

(-> image-input-recognize-pasted-path (string) (option pathname))
(defun image-input-recognize-pasted-path (text)
  "Return the supported image named by pasted TEXT, or NIL."
  (handler-case
      (let ((pathname (image-input-normalize-pasted-path text)))
        (and pathname (image-input-validate-pathname pathname)))
    (image-input-error ()
      nil)
    (error ()
      nil)))

(-> image-input-validate-pathname ((or string pathname)) pathname)
(defun image-input-validate-pathname (location)
  "Return LOCATION as an absolute supported image pathname, or signal."
  (let ((pathname
          (handler-case
              (uiop:ensure-pathname (platform-pathname location)
                                    :defaults (uiop:getcwd)
                                    :ensure-absolute t
                                    :want-non-wild t)
            (error (condition)
              (image-input--error
               (pathname location)
               ':recognition
               (format nil "Image location ~A is not a valid local pathname."
                       location)
               condition)))))
    (multiple-value-bind (format width height)
        (image-input--inspect pathname)
      (declare (ignore format))
      (if (and (plusp width) (plusp height))
          (platform-truename *platform* pathname)
          (image-input--error
           pathname ':recognition
           (format nil "Image ~A has no valid pixel dimensions." pathname))))))


;;;; -- Durable Projection --

(-> image-attachment-record (image-attachment) list)
(defun image-attachment-record (attachment)
  "Return ATTACHMENT's portable durable descriptor."
  (list :id (image-attachment-identifier attachment)
        :artifact (file-namestring (image-attachment-pathname attachment))
        :source (image-attachment-source-name attachment)
        :mime (image-attachment-mime-type attachment)
        :detail "high"
        :width (image-attachment-width attachment)
        :height (image-attachment-height attachment)))

(-> image-input--artifact-name-p (string string) boolean)
(defun image-input--artifact-name-p (name identifier)
  "Return true when artifact NAME is a safe basename for IDENTIFIER."
  (let ((pathname (pathname name)))
    (and (string= (file-namestring pathname) name)
         (string= (or (pathname-name pathname) "") identifier)
         (member (string-downcase (or (pathname-type pathname) ""))
                 '("png" "jpg" "webp")
                 :test #'string=)
         (member (pathname-directory pathname) '(nil (:relative)) :test #'equal)
         (null (pathname-device pathname)))))

(-> image-attachment-from-record (list pathname) image-attachment)
(defun image-attachment-from-record (record artifact-root)
  "Validate durable attachment RECORD beneath ARTIFACT-ROOT."
  (let* ((identifier (getf record :id))
         (artifact (getf record :artifact))
         (source (getf record :source))
         (mime (getf record :mime))
         (detail (getf record :detail))
         (width (getf record :width))
         (height (getf record :height)))
    (unless (and (non-empty-string-p identifier)
                 (non-empty-string-p artifact)
                 (image-input--artifact-name-p artifact identifier)
                 (non-empty-string-p source)
                 (member mime '("image/png" "image/jpeg" "image/webp")
                         :test #'string=)
                 (string= (or detail "") "high")
                 (typep width '(integer 1))
                 (typep height '(integer 1)))
      (image-input--error
       artifact-root ':loading
       "A persisted conversation image descriptor is malformed."))
    (let ((pathname (merge-pathnames artifact artifact-root)))
      (unless (probe-file pathname)
        (image-input--error
         pathname ':loading
         (format nil "Conversation image artifact ~A is missing." pathname)))
      (make-instance 'image-attachment
                     :identifier identifier
                     :pathname pathname
                     :source-name source
                     :mime-type mime
                     :width width
                     :height height))))

(-> image-input--data-url (image-attachment) string)
(defun image-input--data-url (attachment)
  "Return ATTACHMENT as a base64 data URL for one provider request."
  (yolokuva:image-data-url
   (yolokuva:image-read-octets (image-attachment-pathname attachment)
                               :maximum-octets *image-input-maximum-source-bytes*)
   (image-attachment-mime-type attachment)))

(-> image-input-content-item (image-attachment) json-object)
(defun image-input-content-item (attachment)
  "Return ATTACHMENT as one Codex-compatible provider image item."
  (clinker-transcript:input-image-item (image-input--data-url attachment)))

(-> image-input-content-items (image-attachment integer) list)
(defun image-input-content-items (attachment label-number)
  "Return Codex-compatible provider content for ATTACHMENT labelled LABEL-NUMBER."
  (list
   (clinker-transcript:input-text-item
    (format nil "<image name=[Image #~D] path=\"~A\">"
            label-number
            (image-attachment-source-name attachment)))
   (image-input-content-item attachment)
   (clinker-transcript:input-text-item "</image>")))
