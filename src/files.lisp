;;;; The directories where Corona stores its files, and tools for verifying
;;;; their integrity
(in-package :cl-user)
(defpackage corona.files
  (:use :cl)
  (:export :+corona-directory+
           :+vagrant-cloud-directory+
           :+vm-directory+
           :verify-file
           :download
           :extract-tarball
           :copy-files-to-directory))
(in-package :corona.files)

;;; File directories

(defparameter +corona-directory+
  #-corona-testing
  (merge-pathnames #p".corona/"
                   (user-homedir-pathname))
  #+corona-testing
  (asdf:system-relative-pathname :corona #p"t/corona-files/")
  "The directory where Corona stores everything it needs. By default, this is
  `~/.corona`, but when testing this is overriden to the path to the Corona
  system definition, plus `t/corona`.")

(defparameter +files-directory+
  (merge-pathnames #p"files/" +corona-directory+)
  "The directory where Corona stores files (CD/DVD images, virtual machine
  images, virtual hard drives, etc.)")

(defparameter +vagrant-cloud-directory+
  (merge-pathnames #p"vagrant-cloud/" +files-directory+)
  "The directory where Corona stores Vagrant Cloud images.")

(defparameter +vm-directory+
  (merge-pathnames #p"virtual-machines/" +files-directory+)
  "The directory where images of specific virtual machines are stored.")

;;; File verification

(define-condition checksum-mismatch (error)
  ((path :reader path :initarg :path)
   (checksum-type :reader checksum-type :initarg :checksum-type)
   (file-sum :reader file-sum :initarg :file-sum)
   (trusted-sum :reader trusted-sum :initarg :trusted-sum))

  (:report
   (lambda (condition stream)
     (format stream
             "Checksum mismatch: The file ~S has ~A checksum ~S, which differs from the trusted checksum ~S."
             (path condition)
             (checksum-type condition)
             (file-sum condition)
             (trusted-sum condition)))))

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
  (let ((file-checksum (file-checksum checksum-type pathname)))
    (if (equal file-checksum checksum)
        t
        (error 'checksum-mismatch
               :path pathname
               :checksum-type checksum-type
               :file-checksum file-checksum
               :trusted-checksum checksum))))

;;; File downloads

(defun have-curl-p ()
 (equal 0
        (third
         (multiple-value-list (uiop:run-program "which curl"
                                                :ignore-error-status t)))))

(defun download-over-curl (url pathname)
  (ensure-directories-exist
   (cl-fad:pathname-directory-pathname pathname))
  (uiop:run-program
   (format nil "curl -o ~S ~S" (namestring pathname) url)))

(defun download (url pathname)
  "Download the file from `url` to its pathname if it doesn't exist, returning
`t`. If it already exists, return `nil`."
  (if (not (probe-file pathname))
      (progn
        ;; trivial-download is pretty slow for large data, so we cat cheat and
        ;; use curl where available.
        (trivial-download:download url pathname)
        t)
      nil))

;;; File copying

(defun copy-files-to-directory (files destination)
  "Copy a list of files to the `destination` directory."
  (let* ((destination-paths
           (loop for path in files collecting
             (make-pathname :name (pathname-name path)
                            :type (pathname-type path)
                            :defaults destination)))
         (pairs (mapcar #'(lambda (a b) (list a b))
                        files
                        destination-paths)))
    (ensure-directories-exist destination)
    (loop for (source destination) in pairs do
      (cl-fad:copy-file source destination))))
