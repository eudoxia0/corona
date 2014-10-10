;;;; Defining virtual machines (instances of a box) and building them
(in-package :cl-user)
(defpackage corona.vm
  (:use :cl)
  (:import-from :corona.sys
                :<system>
                :box)
  (:export :<hardware>
           :memory
           :cpu-count
           :<vm>
           :name
           :system
           :hardware
           :gb
           :vm-directory
           :already-built-p
           :build-vm
           :setup-vm
           :start
           :stop
           :pause
           :resume
           :reboot
           :poweroff))
(in-package :corona.vm)

(defclass <hardware> ()
  ((memory :reader memory
           :initarg :memory
           :type integer
           :initform 1024
           :documentation "The virtual memory in megabytes.")
   (cpu-count :reader cpu-count
              :initarg :cpu-count
              :type integer
              :initform 1
              :documentation "The number of virtual CPUs."))
  (:documentation "Virtual machine hardware. Some of these are probably
  VirtualBox-specific."))

(defun gb (amount)
  "Convert `amount` gigabytes to megabytes."
  (* amount 1000))

(defclass <vm> ()
  ((name :reader name
         :initarg :name
         :type symbol
         :documentation "The virtual machine name.")
   (system :reader system
           :initarg :system
           :type <system>
           :documentation "The system this VM holds.")
   (hardware :reader hardware
             :initarg :hardware
             :type <hardware>
             :documentation "The virtual hardware."))
  (:documentation "A virtual machine is an instance of a system's base box with
  its own virtual drive and resources."))

(defmethod stored-name ((vm <vm>))
  "The name of the virtual machine that VirtualBox knows."
  (concatenate 'string
               (package-name (symbol-package (name vm)))
               ":"
               (symbol-name (name vm))))

(defmethod vm-directory ((vm <vm>))
  "Directory where VM configuration/disks are stored."
  (merge-pathnames
   (make-pathname :directory (list :relative
                                   (package-name (symbol-package (name vm)))
                                   (symbol-name (name vm))))
   corona.files:+vm-directory+))

(defmethod already-built-p ((vm <vm>))
  "Have we already built the VM?"
  (if (virtualbox:find-by-name (stored-name vm)) t))

;;; The way we build virtual machienes from base boxes is roughly:
;;;
;;;  1. Make sure the box is downloaded and extracted.
;;;  2. Copy the virtual drive and OVF to a folder we control.
;;;  3. Import the copied OVF file.

(defmethod build-vm ((vm <vm>))
  "Build a virtual machine if it's not yet built, downloading its base box if
it's not yet available. Returns `t` if everything went as planned, `nil` if the
VM was already built."
  ;; First, ensure we have the box
  (unless (already-built-p vm)
    (corona.sys:ensure-box (system vm))
    (let* ((box (box (system vm)))
           (destination-directory (vm-directory vm))
           (box-contents (cl-fad:list-directory (corona.cloud:box-directory box))))
      ;; Now we have to copy everything in box-contents to destination-directory
      (log:info "Copying OVF file to Corona directory.")
      (handler-case
          (corona.files:copy-files-to-directory box-contents
                                                destination-directory)
        (t () (log:info "OVF file already copied.")))
      (let ((ovf-file (first
                       (remove-if-not #'(lambda (path)
                                          (equal (pathname-type path) "ovf"))
                                      box-contents))))
        ;; And now, finally, actually issue the import command using
        (log:info "Importing OVF file.")
        ;; cl-virtualbox If we're testing, don't actually import anything
        #-corona-testing
        (virtualbox:import-vm ovf-file (stored-name vm))
        t))))

(defmethod setup-vm ((vm <vm>))
  "Apply the virtual machine's virtual hardware settings."
  (when (slot-boundp vm 'hardware)
    (let ((hardware (hardware vm))
          (name (stored-name vm)))
      (virtualbox:set-vm-memory (memory hardware) name)
      (virtualbox:set-vm-cpu-count (cpu-count hardware) name))))

(defmethod start ((vm <vm>))
  (log:info "Starting...")
  (virtualbox:start-vm (stored-name vm)))

(defmethod stop ((vm <vm>))
  (log:info "Stopping...")
  (poweroff vm))

(defmethod pause ((vm <vm>))
  (log:info "Pausing...")
  (virtualbox:pause-vm (stored-name vm)))

(defmethod resume ((vm <vm>))
  (log:info "Resuming...")
  (virtualbox:resume-vm (stored-name vm)))

(defmethod reboot ((vm <vm>))
  (log:info "Rebooting...")
  (virtualbox:cold-reboot-vm (stored-name vm)))

(defmethod poweroff ((vm <vm>))
  (log:info "Forcing shutdown...")
  (virtualbox:poweroff-vm (stored-name vm)))
