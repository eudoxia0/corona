;;;; The main interface
(in-package :cl-user)
(defpackage corona
  (:use :cl)
  (:import-from :corona.vm
                :start
                :stop)
  (:export :defmachine
           :start
           :stop))
(in-package :corona)

(defmacro defmachine (name &key system (memory 512) (cpu-count 1) ip)
  `(defparameter ,name
     (make-instance 'corona.vm:<vm>
                    :name ',name
                    :system (corona.sys:find-system ,@system)
                    :hardware (make-instance 'corona.vm:<hardware>
                                             :memory ,memory
                                             :cpu-count ,cpu-count
                                             :ip ,ip))))
