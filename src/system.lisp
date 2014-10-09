;;;; Defining systems, and a list of known systems we can pull from Vagrant Cloud
(in-package :cl-user)
(defpackage corona.sys
  (:use :cl)
  (:import-from :corona.cloud
                :<cloud-box>)
  (:export :<system>
           :name
           :version
           :arch
           :cloud-box
           :list-system-names
           :list-versions-for-system
           :list-archs-for-system
           :list-systems))
(in-package :corona.sys)

(defclass <system> ()
  ((name :reader name
         :initarg :name
         :type keyword
         :documentation "The name of the operating system,
         e.g. :ubuntu, :windows.")
   (version :reader version
            :initarg :version
            :type keyword
            :documentation "The operating system's version string as a keyword,
            e.g. :13.13.")
   (arch :reader arch
         :initarg :arch
         :type keyword
         :documentation "The operating system's architecture as a keyword,
         e.g. :64, :sparc.")
   (cloud-box :reader cloud-box
              :initarg :cloud-box
              :type corona.cloud:<cloud-box>
              :documentation "The box that bootstraps this system.")))

;;; Known systems

(defparameter +known-systems+ (list)
  "A list of available systems.")

(defmacro define-system (&rest params)
  `(push (make-instance '<system> ,@params) +known-systems+))

(defmacro define-box (&rest params)
  `(make-instance '<cloud-box> ,@params))

(defun list-system-names ()
  "List the names of available systems."
  (remove-duplicates
   (loop for system in +known-systems+ collecting (name system))))

(defun list-versions-for-system (name)
  "List the available versions of a given system."
  (remove-duplicates
   (loop for system in +known-systems+
     if (eq name (name system)) collect (version system))))

(defun list-archs-for-system (name)
  "List the available architectures of a given system."
  (remove-duplicates
   (loop for system in +known-systems+
     if (eq name (name system)) collect (arch system))))

(defun list-systems (&optional (stream *standard-output*))
  "Return a summary of available systems, their versions and architectures."
  (loop for system-name in (list-system-names) do
    (let ((versions (list-versions-for-system system-name))
          (archs (list-archs-for-system system-name)))
      )))
