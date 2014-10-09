(in-package :cl-user)
(defpackage corona-asd
  (:use :cl :asdf))
(in-package :corona-asd)

(defsystem corona
  :version "0.1"
  :author "Fernando Borretti"
  :license "MIT"
  :depends-on (:cl-virtualbox
               :trivial-download
               :ironclad
               :trivial-types
               :archive)
  :components ((:module "src"
                :serial t
                :components
                ((:file "files")
                 (:file "vagrant-cloud")
                 (:file "system")
                 (:file "system-list"))))
  :description "Isolated, reproducible virtual development environments."
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op corona-test))))
