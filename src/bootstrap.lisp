;;;; Bootstrapping is the process of creating a working virtual machine from an
;;;; ISO file. We download and verify the file from the Internet, create a
;;;; temporary bootstrapping virtual machine, then mount the ISO and install it
;;;; by sending virtual keypresses to the machine. Then, we export the state of
;;;; the virtual machine and destroy the bootstrapping environment.
(in-package :cl-user)
(defpackage corona.bootstrap
  (:use :cl))
(in-package :corona.bootstrap)

(defparameter +corona-directory+
  (merge-pathnames #p".config/corona/"
                   (user-homedir-pathname))
  "The directory where Corona stores everything it needs.")

(defparameter +files-directory+
  (merge-pathnames #p"files/" +corona-directory+)
  "The directory where Corona stores files (CD/DVD images, virtual machine
  images, virtual hard drives, etc.)")

(defparameter +media-directory+
  (merge-pathnames #p"media/" +files-directory+)
  "The directory where Corona stores installation media (CD/DVD images).")

(defun url-slug (url)
  "Extract the slug from a URL."
  (first (last (puri:uri-parsed-path (puri:parse-uri url)))))

(defun media-url-pathname (url)
  "The pathname where the contents of `url` will be stored. `url` is an URL to a
CD/DVD image."
  (merge-pathnames (parse-namestring (url-slug url))
                   +media-directory+))

(defun download-media (url)
  "Download the installation media from `url` to its pathname if it doesn't
exist."
  (let ((local-pathname (media-url-pathname url)))
    (unless (probe-file local-pathname)
      (trivial-download:download url local-pathname))))

(defun file-checksum (checksum-type pathname)
  "Generate the checksum of a file."
  (let* ((array (make-array 4096 :element-type '(unsigned-byte 8)))
         (digester (ironclad:make-digest checksum-type))
         (digest (make-array (ironclad:digest-length checksum-type)
                             :element-type '(unsigned-byte 8))))
    (ironclad:digest-file digester pathname :buffer array :digest digest)
    (ironclad:byte-array-to-hex-string digest)))

(defun verify-file (pathname checksum-type checksum)
  "Verify the `checksum-type` checksum of the file at `pathname` is equal to
`checksum`."
  (equal (file-checksum checksum-type pathname) checksum))
