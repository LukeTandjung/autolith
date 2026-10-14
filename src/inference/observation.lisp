(in-package #:autolith)


(defgeneric rlm-call-with-observation
    (kind callback function)
  (:documentation
   "Run FUNCTION with an activity CALLBACK within an inference lifecycle of KIND."))


(defmethod rlm-call-with-observation ((kind symbol) callback function)
  (declare (ignore kind))
  (funcall function callback))


(defgeneric rlm-note-conversation
    (conversation)
  (:documentation
   "Publish an inference's durable CONVERSATION identity before its first request."))


(defmethod rlm-note-conversation ((conversation conversation)) conversation)


(defgeneric rlm-inherit-activity-callback
    (callback &key parent viewer)
  (:documentation
   "Associate CALLBACK with its PARENT callback or owning VIEWER without copying request data."))


(defmethod rlm-inherit-activity-callback (callback &key parent viewer)
  (declare (ignore parent viewer))
  callback)
