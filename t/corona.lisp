(in-package :cl-user)
(defpackage corona-test
  (:use :cl :fiveam))
(in-package :corona-test)

;;; Variables

(defparameter +tmp-directory+
  (asdf:system-relative-pathname :corona #p"tmp/"))

(defparameter +test-tar-url+
  "http://ftp5.usa.openbsd.org/pub/OpenBSD/OpenSSH/portable/openssh-2.1.1p4.tar.gz"
  "To test that downloading files and extracting archives works, we use an
OpenSSH tarball. This URL should exist for the foreseeable future.")

(defparameter +test-tar-pathname+
  (merge-pathnames #p"test.tar.gz" +tmp-directory+))

;;; Test suites

(def-suite corona)
(in-suite corona)

(test testing-environment
  (is-true
   #+corona-testing t #-corona-testing nil)
  (is (equal corona.files:+corona-directory+
             (asdf:system-relative-pathname :corona #p"t/corona-files/"))))

(test downloading-files
  (finishes
    ;; Create directory
    (ensure-directories-exist +tmp-directory+))
  (is-true
   (corona.files:download +test-tar-url+ +test-tar-pathname+))
  (is-false
   (corona.files:download +test-tar-url+ +test-tar-pathname+))
  (is-true
   (delete-file +test-tar-pathname+))
  (is-false
   (probe-file +test-tar-pathname+)))

(run! 'corona)

(remove :corona-testing *features*)
