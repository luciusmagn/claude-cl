(ql:quickload :claude-api)

(defpackage #:my-claude-app
  (:use #:cl :claude-api))

(in-package #:my-claude-app)

(defun chat-with-claude (prompt)
  (let* ((message (make-message +role-user+
                                (list (make-text-content prompt))))
         (request (make-ai-request :model "claude-3-7-sonnet-20250219"
                                   :messages (list message)
                                   :max-tokens 4096
                                   :api-key "your-api-key-here")))
    (let ((response (send-request request)))
      (if (typep response 'error-response)
          (format t "Error: ~A~%" (error-message response))
          (dolist (content (response-content response))
            (when (string= (content-type content) +content-type-text+)
              (format t "~A~%" (text content))))))))
