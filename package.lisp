(defpackage #:claude-api
  (:use #:cl #:jonathan)
  (:export ;; Content types
   #:+content-type-text+
   #:+content-type-image+
   #:+content-type-tool-use+
   #:+content-type-tool-result+

   ;; Roles
   #:+role-user+
   #:+role-assistant+

   ;; Classes
   #:content
   #:message
   #:metadata
   #:ai-request
   #:ai-response
   #:error-response

   ;; Accessors
   #:text
   #:content-type
   #:response-content
   #:error-message

   ;; Constructors
   #:make-text-content
   #:make-image-content
   #:make-tool-use-content
   #:make-tool-result-content
   #:make-message
   #:make-ai-request

   ;; Main function
   #:send-request))
