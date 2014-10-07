;;;; Base boxes are virtual machine images that come with the operating system
;;;; and a user account already set up. Defining a base box requires pointing to
;;;; the URL of its installation medium, providing checksums for security, and
;;;; defining the list of commands that need to be sent to the VM to install
;;;; it. These commands are exclusively virtual keypresses, since there's no SSH
;;;; server before an OS is installed.
(in-package :cl-user)
(defpackage corona.base
  (:use :cl :trivial-types))
(in-package :corona.base)

(defclass <base-box> ()
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
         e.g. :amd64, :sparc.")
   (source-url :reader source-url
               :initarg :source-url
               :type string
               :documentation "The URL to the installation medium.")
   (checksum-type :reader checksum-type
                  :initarg :checksum-type
                  :type :keyword
                  :documentation "The type of checksum used to verify the
                  download, e.g. :sha1, :md5.")
   (checksum :reader checksum
             :initarg :checksum
             :type string))
  (:documentation "A base box."))
