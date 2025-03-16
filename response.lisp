(in-package #:claude-api)

;; Define usage class
(defclass usage ()
  ((input-tokens  :initarg :input-tokens  :accessor input-tokens  :type integer)
   (output-tokens :initarg :output-tokens :accessor output-tokens :type integer)))

;; Define AI response class
(defclass ai-response ()
  ((id            :initarg :id            :accessor response-id            :type string)
   (response-type :initarg :response-type :accessor response-type         :type string)
   (role          :initarg :role          :accessor response-role          :type string)
   (content       :initarg :content       :accessor response-content       :type list)
   (model         :initarg :model         :accessor response-model         :type string)
   (stop-reason   :initarg :stop-reason   :accessor response-stop-reason   :initform nil)
   (stop-sequence :initarg :stop-sequence :accessor response-stop-sequence :initform nil)
   (usage         :initarg :usage         :accessor response-usage)))

;; Define error response class
(defclass error-response ()
  ((response-type :initarg :response-type :accessor error-response-type :type string)
   (error-type    :initarg :error-type    :accessor error-type         :type string)
   (error-message :initarg :error-message :accessor error-message      :type string)))

;; Parser functions
(defun parse-content (content-data)
  (let ((content-type (cdr (assoc "type" content-data :test #'string=))))
    (cond
      ((string= content-type "text")
       (make-text-content (cdr (assoc "text" content-data :test #'string=))))
      ((string= content-type "image")
       (let ((source (cdr (assoc "source" content-data :test #'string=))))
         (make-image-content
          (cdr (assoc "media_type" source :test #'string=))
          (cdr (assoc "data" source :test #'string=)))))
      ((string= content-type "tool_use")
       (make-tool-use-content
        (cdr (assoc "id" content-data :test #'string=))
        (cdr (assoc "name" content-data :test #'string=))
        (cdr (assoc "input" content-data :test #'string=))))
      ((string= content-type "tool_result")
       (make-tool-result-content
        (cdr (assoc "tool_use_id" content-data :test #'string=))
        (cdr (assoc "content" content-data :test #'string=))
        (cdr (assoc "is_error" content-data :test #'string=)))))))

(defun parse-usage (usage-data)
  (make-instance 'usage
                 :input-tokens  (cdr (assoc "input_tokens" usage-data :test #'string=))
                 :output-tokens (cdr (assoc "output_tokens" usage-data :test #'string=))))

(defun parse-response (json-data)
  (let ((response-type (cdr (assoc "type" json-data :test #'string=))))
    (if (string= response-type "error")
        (let ((error-data (cdr (assoc "error" json-data :test #'string=))))
          (make-instance 'error-response
                         :response-type response-type
                         :error-type    (cdr (assoc "type" error-data :test #'string=))
                         :error-message (cdr (assoc "message" error-data :test #'string=))))
        (make-instance 'ai-response
                       :id            (cdr (assoc "id" json-data :test #'string=))
                       :response-type response-type
                       :role          (cdr (assoc "role" json-data :test #'string=))
                       :content       (mapcar #'parse-content (cdr (assoc "content" json-data :test #'string=)))
                       :model         (cdr (assoc "model" json-data :test #'string=))
                       :stop-reason   (cdr (assoc "stop_reason" json-data :test #'string=))
                       :stop-sequence (cdr (assoc "stop_sequence" json-data :test #'string=))
                       :usage         (parse-usage (cdr (assoc "usage" json-data :test #'string=)))))))
