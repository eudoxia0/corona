;;;; Defining systems, and a list of known systems we can pull from Vagrant Cloud
(in-package :cl-user)
(defpackage corona.sys
  (:use :cl)
  (:import-from :corona.cloud
                :<cloud-box>)
  (:export :<system>))
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
