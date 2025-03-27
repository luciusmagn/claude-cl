(in-package #:claude-api)

;; Constants
(defparameter +content-type-text+ "text")
(defparameter +content-type-image+ "image")
(defparameter +content-type-tool-use+ "tool_use")
(defparameter +content-type-tool-result+ "tool_result")

(defparameter +role-user+ "user")
(defparameter +role-assistant+ "assistant")

;; Content class
(defclass content ()
  ((content-type :initarg :content-type :accessor content-type :type string)
   (text         :initarg :text         :accessor text         :initform nil)
   (source-type  :initarg :source-type  :accessor source-type  :initform nil)
   (media-type   :initarg :media-type   :accessor media-type   :initform nil)
   (data         :initarg :data         :accessor data         :initform nil)
   (id           :initarg :id           :accessor id           :initform nil)
   (name         :initarg :name         :accessor name         :initform nil)
   (input        :initarg :input        :accessor input        :initform nil)
   (tool-use-id  :initarg :tool-use-id  :accessor tool-use-id  :initform nil)
   (is-error     :initarg :is-error     :accessor is-error     :initform nil)))

;; Content constructors
(defun make-text-content (text)
  (make-instance 'content
                 :content-type +content-type-text+
                 :text         text))

(defun make-image-content (media-type data)
  (make-instance 'content
                 :content-type +content-type-image+
                 :source-type  "base64"
                 :media-type   media-type
                 :data         data))

(defun make-tool-use-content (id name input)
  (make-instance 'content
                 :content-type +content-type-tool-use+
                 :id           id
                 :name         name
                 :input        input))

(defun make-tool-result-content (tool-use-id content is-error)
  (make-instance 'content
                 :content-type +content-type-tool-result+
                 :tool-use-id  tool-use-id
                 :data         content
                 :is-error     is-error))

;; Message class
(defclass message ()
  ((role    :initarg :role    :accessor message-role    :type string)
   (content :initarg :content :accessor message-content :type list)))

(defun make-message (role content-list)
  (make-instance 'message
                 :role    role
                 :content content-list))

;; Tool classes
(defclass tool ()
  ((name         :initarg :name         :accessor tool-name         :type string)
   (description  :initarg :description  :accessor tool-description  :type string)
   (input-schema :initarg :input-schema :accessor tool-input-schema :initform nil)))

(defclass tool-choice ()
  ((choice-type :initarg :choice-type :accessor choice-type :type string)
   (name        :initarg :name        :accessor choice-name :initform nil)))

;; Metadata class
(defclass metadata  ()
  ((user-id :initarg :user-id :accessor user-id)))
