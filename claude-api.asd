(asdf:defsystem #:claude-api
  :description "Common Lisp client for Anthropic's Claude API"
  :version "0.1.0"
  :depends-on (#:dexador        ; HTTP client
               #:jonathan       ; JSON parsing
               #:alexandria     ; Utilities
               #:uuid)          ; For UUIDs
  :components ((:file "package")
               (:file "content" :depends-on ("package"))
               (:file "request" :depends-on ("content"))
               (:file "response" :depends-on ("content" "request"))))
