(in-package :cl-user)
(defpackage corona-asd
  (:use :cl :asdf))
(in-package :corona-asd)

(defsystem corona
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :maintainer "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :version "0.1"
  :homepage "http://eudoxia.me/corona"
  :bug-tracker "https://github.com/eudoxia0/corona/issues"
  :source-control (:git "git@github.com:eudoxia0/corona.git")
  :depends-on (:cl-virtualbox
               :trivial-download
               :trivial-types
               :trivial-extract
               :ironclad
               :cl-fad
               :log4cl
               :anaphora)
  :components ((:module "src"
                :serial t
                :components
                ((:file "files")
                 (:file "vagrant-cloud")
                 (:file "system")
                 (:file "system-list")
                 (:file "virtual-machines")
                 (:file "corona"))))
  :description "Isolated, reproducible virtual development environments."
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op corona-test))))
