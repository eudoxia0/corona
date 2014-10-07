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
               :puri
               :ironclad)
  :components ((:module "src"
                :components
                ((:file "bootstrap"))))
  :description "Isolated, reproducible virtual development environments."
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op corona-test))))
