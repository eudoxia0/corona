;;;; Defining particular virtual machines
(in-package :cl-user)
(defpackage corona.vm
  (:use :cl))
(in-package :corona.vm)

(defclass <vm> ()
  ((system :reader system
           :initarg :system
           :type corona.sys:<system>
           :documentation "The system this VM holds."))
  (:documentation "A virtual machine is an instance of a system's base box with
  its own virtual drive and resources."))
