;;;; Tools for downloading virtual machines from Vagrant Cloud
(in-package :cl-user)
(defpackage corona.cloud
  (:use :cl)
  (:export :<cloud-box>))
(in-package :corona.cloud)

(defparameter +vagrant-cloud-url-fmt+
  "https://vagrantcloud.com/~A/boxes/~A/versions/~A/providers/virtualbox.box"
  "The format control string for a Vagrant Cloud box URL. The first parameter is
  the username, then the box name, and finally the version number.")

(defclass <cloud-box> ()
  ((name :reader name
         :initarg :name
         :type string
         :documentation "The box name in Vagrant Cloud.")
   (author :reader author
           :initarg :author
           :type string
           :documentation "The author name in Vagrant Cloud.")
   (version :reader version
            :initarg :version
            :type integer
            :initform 1
            :documentation "The box version in Vagrant Cloud.")
   (checksum-type :reader checksum-type
                  :initarg :checksum-type
                  :type :keyword
                  :initform nil
                  :documentation "The type of checksum used to verify the
                  download, e.g. :sha1, :md5.")
   (checksum :reader checksum
             :initarg :checksum
             :initform nil
             :type string
             :documentation "The checksum string."))
  (:documentation "A base box."))

;;; Downloading boxes

(defmethod source-url ((box <cloud-box>))
  "URL to download a cloud box."
  (format nil +vagrant-cloud-url-fmt+ (author box) (name box) (version box)))

(defmethod box-directory ((box <cloud-box>))
  "Directory where a box contents would be/are stored."
  (merge-pathnames (parse-namestring
                    (format nil "~A-~A-~A/"
                            (author box)
                            (name box)
                            (version box)))
                   corona.files:+vagrant-cloud-directory+))

;;; Once we have a local copy of a box, we have to extract the VirtualBox
;;; image. A Vagrant box file is just a .zip, which internally looks like this:
;;;
;;; name.box/
;;;   Vagrantfile
;;;   box.ovf
;;;   box-disk1.vmdk
;;;
;;; The OVF file is what we have to import, and the VMDK file is the virtual
;;; HDD.

(defmethod download-and-extract-box ((box <cloud-box>))
  (let ((url (source-url box))
        (archive-path (merge-pathnames #p"box-file.box"
                                       (box-directory box))))
    ;;; First things first. We download the box.
    (corona.files:download url archive-path)
    ;; Then we verify the checksums, if any
    (with-slots (checksum-type checksum) box
      (if (and checksum-type checksum)
          (corona.files:verify-file archive-path checksum-type checksum)))
    ;; Now that it's passed verification we extract the tarball
    (corona.files:extract-tarball archive-path)
    ;; Since we don't want to duplicate content, we delete the archive once it's
    ;; extracted
    (delete-file archive-path)
    ;; We also delete the Vagrantfile, which we don't use
    (let ((vagrantfile (make-pathname :name "Vagrantfile"
                                      :directory (pathname-directory archive-path))))
      (when (probe-file vagrantfile)
        (delete-file vagrantfile)))))
