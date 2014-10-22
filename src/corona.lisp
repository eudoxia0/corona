;;;; The main interface
(in-package :cl-user)
(defpackage corona
  (:use :cl)
  (:import-from :corona.vm
                :start
                :stop
                :pause
                :resume
                :reboot
                :poweroff)
  (:export :defmachine
           :start
           :stop
           :pause
           :resume
           :reboot
           :poweroff))
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
