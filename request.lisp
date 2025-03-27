(in-package #:claude-api)

;; AI Request class
(defclass ai-request ()
  ((model             :initarg :model             :accessor request-model             :type string)
   (messages          :initarg :messages          :accessor request-messages          :type list)
   (max-tokens        :initarg :max-tokens        :accessor request-max-tokens        :type integer)
   (metadata          :initarg :metadata          :accessor request-metadata          :initform nil)
   (stream            :initarg :stream            :accessor request-stream            :initform nil)
   (system            :initarg :system            :accessor request-system            :initform nil)
   (temperature       :initarg :temperature       :accessor request-temperature       :initform nil)
   (top-k             :initarg :top-k             :accessor request-top-k             :initform nil)
   (top-p             :initarg :top-p             :accessor request-top-p             :initform nil)
   (api-key           :initarg :api-key           :accessor request-api-key           :type string)
   (anthropic-version :initarg :anthropic-version :accessor request-anthropic-version :initform "2023-06-01" :type string)
   (tools             :initarg :tools             :accessor request-tools             :initform nil)
   (tool-choice       :initarg :tool-choice       :accessor request-tool-choice       :initform nil)))

;; Request constructor with keyword arguments
(defun make-ai-request (&key model messages max-tokens metadata stream system
                          temperature top-k top-p api-key (anthropic-version "2023-06-01")
                          tools tool-choice)
  (make-instance 'ai-request
                 :model             model
                 :messages          messages
                 :max-tokens        max-tokens
                 :metadata          metadata
                 :stream            stream
                 :system            system
                 :temperature       temperature
                 :top-k             top-k
                 :top-p             top-p
                 :api-key           api-key
                 :anthropic-version anthropic-version
                 :tools             tools
                 :tool-choice       tool-choice))

;; Conversion functions for JSON serialization
(defun content-to-plist (content)
  (case (content-type content)
    ("text"
     (list :type (content-type content) :text (text content)))
    ("image"
     (list :type (content-type content)
           :source (list :type       (source-type content)
                         :media_type (media-type content)
                         :data       (data content))))
    ("tool_use"
     (list :type  (content-type content)
           :id    (id content)
           :name  (name content)
           :input (input content)))
    ("tool_result"
     (list :type        (content-type content)
           :tool_use_id (tool-use-id content)
           :content     (data content)
           :is_error    (is-error content)))))

(defun message-to-plist (message)
  (list :role    (message-role message)
        :content (mapcar #'content-to-plist (message-content message))))

(defun tool-to-plist (tool)
  (let ((result (list :name        (tool-name tool)
                      :description (tool-description tool))))
    (when (tool-input-schema tool)
      (setf (getf result :input_schema) (tool-input-schema tool)))
    result))

(defun tool-choice-to-plist (tool-choice)
  (let ((result (list :type (choice-type tool-choice))))
    (when (choice-name tool-choice)
      (setf (getf result :name) (choice-name tool-choice)))
    result))

(defun request-to-plist (request)
  (let ((result (list :model      (request-model request)
                      :messages   (mapcar #'message-to-plist (request-messages request))
                      :max_tokens (request-max-tokens request))))

    ;; Add optional fields if present
    (when (request-metadata request)
      (setf (getf result :metadata)
            (list :user_id (user-id (request-metadata request)))))

    (when (request-stream request)
      (setf (getf result :stream) (request-stream request)))

    (when (request-system request)
      (setf (getf result :system) (request-system request)))

    (when (request-temperature request)
      (setf (getf result :temperature) (request-temperature request)))

    (when (request-top-k request)
      (setf (getf result :top_k) (request-top-k request)))

    (when (request-top-p request)
      (setf (getf result :top_p) (request-top-p request)))

    (when (request-tools request)
      (setf (getf result :tools) (mapcar #'tool-to-plist (request-tools request))))

    (when (request-tool-choice request)
      (setf (getf result :tool_choice) (tool-choice-to-plist (request-tool-choice request))))

    result))

(defmethod jonathan:%to-json ((content content))
  (with-object
    (write-key-value "type" (content-type content))
    (let ((content-type-str (content-type content)))
      (cond
        ((string= content-type-str "text")
         (write-key-value "text" (text content)))
        ((string= content-type-str "image")
         (write-key "source")
         (with-object
           (write-key-value "type" (source-type content))
           (write-key-value "media_type" (media-type content))
           (write-key-value "data" (data content))))
        ((string= content-type-str "tool_use")
         (write-key-value "id" (id content))
         (write-key-value "name" (name content))
         (write-key-value "input" (input content)))
        ((string= content-type-str "tool_result")
         (write-key-value "tool_use_id" (tool-use-id content))
         (write-key-value "content" (data content))
         (write-key-value "is_error" (is-error content)))))))

(defmethod jonathan:%to-json ((message message))
  (with-object
    (write-key-value "role" (message-role message))
    (write-key-value "content"
                     (with-array
                       (dolist (content (message-content message))
                         (write-item content))))))

(defmethod jonathan:%to-json ((request ai-request))
  (with-object
    (write-key-value "model" (request-model request))
    (write-key-value "max_tokens" (request-max-tokens request))
    (write-key-value "messages"
                     (with-array
                       (dolist (message (request-messages request))
                         (write-item message))))

    ;; Optional fields
    (when (request-metadata request)
      (write-key-value "metadata"
                       (with-object
                         (write-key-value "user_id" (user-id (request-metadata request))))))

    (when (request-stream request)
      (write-key-value "stream" (request-stream request)))

    (when (request-system request)
      (write-key-value "system" (request-system request)))

    (when (request-temperature request)
      (write-key-value "temperature" (request-temperature request)))

    (when (request-top-k request)
      (write-key-value "top_k" (request-top-k request)))

    (when (request-top-p request)
      (write-key-value "top_p" (request-top-p request)))

    (when (request-tools request)
      (write-key-value "tools"
                       (with-array
                         (dolist (tool (request-tools request))
                           (with-object
                             (write-key-value "name" (tool-name tool))
                             (write-key-value "description" (tool-description tool))
                             (when (tool-input-schema tool)
                               (write-key-value "input_schema" (tool-input-schema tool))))))))

    (when (request-tool-choice request)
      (write-key-value "tool_choice"
                       (with-object
                         (write-key-value "type" (choice-type (request-tool-choice request)))
                         (when (choice-name (request-tool-choice request))
                           (write-key-value "name" (choice-name (request-tool-choice request)))))))))

(defun send-request (request)
  (let* ((json-request (jonathan:to-json request))
         (api-key (request-api-key request))
         (anthropic-version (request-anthropic-version request))
         (headers `(("x-api-key"         . ,api-key)
                    ("content-type"      . "application/json")
                    ("anthropic-version" . ,anthropic-version)
                    ("anthropic-beta"    . "max-tokens-3-5-sonnet-2024-07-15"))))

    ;; For debugging
    ;;(format t "~%Sending JSON: ~A~%" json-request)

    (handler-case
        (multiple-value-bind (body status headers)
            (dexador:post "https://api.anthropic.com/v1/messages"
                          :headers headers
                          :content json-request)
          (declare (ignore headers))
          (if (= status 200)
              (parse-response (jonathan:parse body :as :alist))
              (make-instance 'error-response
                             :response-type "error"
                             :error-type    "http_error"
                             :error-message (format nil "HTTP status: ~A~%~A" status body))))

      (error (e)
        (make-instance 'error-response
                       :response-type "error"
                       :error-type    "request_error"
                       :error-message (format nil "Request error: ~A" e))))))
