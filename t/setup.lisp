;;;; We create a fake system, including a fake box, whose source tarball we'll
;;;; build here, from files we'll also generate here.
(in-package :cl-user)
(defpackage corona-test
  (:use :cl :fiveam))
(in-package :corona-test)

;;; The temporary directory will hold the fake box tarball

(defparameter +tmp-dir+
  (asdf:system-relative-pathname :corona #p"tmp/"))

;;; The server will serve files from +tmp-directory+, including the tarball

(defparameter +server+
  (make-instance 'clack.middleware.static:<clack-middleware-static>
                 :path "/"
                 :root +tmp-dir+))

(defparameter *server-handler* nil)

(defparameter +server-port+ 41111)

;;;; We create this class to override the source-url method

(defclass <test-box> (corona.cloud:<cloud-box>) ())

(defmethod corona.cloud:source-url ((box <test-box>))
  (format nil "http://localhost:~A/file.tar" +server-port+))

;;; Here we define the fake system we'll be building

(defparameter +box+
  (make-instance '<test-box>
                 :name "test"
                 :author "test"))

(defparameter +system+
  (make-instance 'corona.sys:<system>
                 :name :test
                 :version :0.1
                 :arch :imaginary
                 :box +box+))

;;; Here are some utilities to generate the box files and tarball

(defun make-fake-file (pathname)
  "A utility function to generate fake files."
  (with-open-file (stream pathname
                          :direction :output
                          :if-does-not-exist :create
                          :if-exists :supersede)
    (write-string "beep boop meaningless data" stream)))

(defun pathname-file (pathname)
  (make-pathname :name (pathname-name pathname)
                 :type (pathname-type pathname)))

(defun make-tarball (tarball-path files)
  (let ((current-dpf *default-pathname-defaults*))
    (setf *default-pathname-defaults*
          (make-pathname :directory (pathname-directory tarball-path)))
    (archive::create-tar-file
     tarball-path
     (loop for file in files collecting (pathname-file file)))
    (setf *default-pathname-defaults* current-dpf)
    t))

;;; Now, we build the actual box.

(defparameter +image-file+
  (merge-pathnames #p"box.ovf" +tmp-dir+))
(defparameter +hdd-file+
  (merge-pathnames #p"box-disk1.vmdk" +tmp-dir+))
(defparameter +vagrantfile+
  (merge-pathnames #p"Vagrantfile" +tmp-dir+))
(defparameter +tarball-path+
  (merge-pathnames #p"file.tar" +tmp-dir+))
(defparameter +tarball-file-list+
  (list +image-file+ +hdd-file+ +vagrantfile+))

;;; Now we define a virtual machine that uses this system

(defparameter +vm+
  (make-instance 'corona.vm:<vm>
                 :name 'test-vm
                 :system +system+))
