;;;; Defining virtual machines (instances of a box) and building them
(in-package :cl-user)
(defpackage corona.vm
  (:use :cl :anaphora)
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

(defparameter +wait-time+ 3)

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
             :documentation "The virtual hardware.")
   (ip :reader ip
       :initarg :ip
       :initform nil
       :type string
       :documentation "The VM's IP address as a string."))
  (:documentation "A virtual machine is an instance of a system's base box with
  its own virtual drive and resources."))

(defmethod stored-name ((vm <vm>))
  "The name of the virtual machine that VirtualBox knows."
  (concatenate 'string
               (package-name (symbol-package (name vm)))
               ":"
               (symbol-name (name vm))))

(defmethod network-name ((vm <vm>))
  "The name of the machine's network."
  (concatenate 'string "vboxnet" (stored-name vm)))

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
      (log:info "Setting VM memory.")
      (virtualbox:set-vm-memory name (memory hardware))
      (log:info "Setting VM CPU count.")
      (virtualbox:set-vm-cpu-count name (cpu-count hardware))
      (aif (ip vm)
           (virtualbox:set-vm-ip (stored-name vm)
                                 (network-name vm)
                                 it)))))

(defmethod ensure-vm ((vm <vm>))
  "Ensure the VM is built an setup."
  (build-vm vm)
  (setup-vm vm))

(defmethod readyp ((vm <vm>))
  "Is the virtual machine ready to accept a login?"
  ;; To test whether the machine is ready, we check the output of
  ;; virtualbox:execute
  (handler-case
      (virtualbox:execute (stored-name vm)
                          "/bin/ls"
                          "vagrant"
                          "vagrant"
                          :wait-stdout t)
    (uiop/run-program:subprocess-error ()
      nil)))

(defmethod wait-for-ready ((vm <vm>))
  "Wait until the machine is ready for operation."
  (loop until (readyp vm) do
    (log:info "Waiting for machine to boot...")
    (sleep +wait-time+)))

(defmethod start ((vm <vm>))
  "Start the VM."
  (ensure-vm vm)
  (log:info "Starting...")
  (virtualbox:start-vm (stored-name vm))
  (wait-for-ready vm))

(defmethod stop ((vm <vm>))
  "Stop the VM."
  (log:info "Stopping...")
  (poweroff vm))

(defmethod pause ((vm <vm>))
  "Pause the VM."
  (log:info "Pausing...")
  (virtualbox:pause-vm (stored-name vm)))

(defmethod resume ((vm <vm>))
  "Resume a paused VM."
  (log:info "Resuming...")
  (virtualbox:resume-vm (stored-name vm)))

(defmethod reboot ((vm <vm>))
  "Reboot the VM."
  (log:info "Rebooting...")
  (virtualbox:cold-reboot-vm (stored-name vm)))

(defmethod poweroff ((vm <vm>))
  "Force the VM to shut down."
  (log:info "Forcing shutdown...")
  (virtualbox:poweroff-vm (stored-name vm)))

(defmethod map-ports ((vm <vm>) host-port guest-port)
  "Map traffic going to `host-port` to `guest-port` if the machine has a
`host`."
  (aif (ip vm)
       (virtualbox:map-vm-ports (stored-name vm)
                                host-port
                                it
                                guest-port)))
