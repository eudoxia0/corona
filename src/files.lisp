;;;; The directories where Corona stores its files, and tools for verifying
;;;; their integrity
(in-package :cl-user)
(defpackage corona.files
  (:use :cl)
  (:export :+vagrant-cloud-directory+
           :verify-file
           :download
           :extract-tarball))
(in-package :corona.files)

;;; File directories

(defparameter +corona-directory+
  #-corona-testing
  (merge-pathnames #p".config/corona/"
                   (user-homedir-pathname))
  #+corona-testing
  (asdf:system-relative-pathname :corona #p"t/corona-files/")
  "The directory where Corona stores everything it needs. By default, this is
  `~/.config/corona`, but when testing this is overriden to the path to the
  Corona system definition, plus `t/corona`.")

(defparameter +files-directory+
  (merge-pathnames #p"files/" +corona-directory+)
  "The directory where Corona stores files (CD/DVD images, virtual machine
  images, virtual hard drives, etc.)")

(defparameter +vagrant-cloud-directory+
  (merge-pathnames #p"vagrant-cloud/" +files-directory+)
  "The directory where Corona stores Vagrant Cloud images.")

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

(defun download (url pathname)
  "Download the file from `url` to its pathname if it doesn't exist."
  (unless (probe-file pathname)
    (trivial-download:download url pathname)))

;;; Archives

(defun extract-tarball (pathname)
  "Extract a tarball to its containing directory."
  ;; archive, by default, extracts all entries to *default-pathname-defaults*,
  ;; so we have to override its value to the directory that contains the archive
  (let ((current-dpf *default-pathname-defaults*))
    (setf *default-pathname-defaults*
          (make-pathname :directory (pathname-directory pathname)))
    (archive:with-open-archive (archive pathname)
      (archive:do-archive-entries (entry archive)
        (archive:extract-entry archive entry)))
    (setf *default-pathname-defaults* current-dpf)
    t))
