(in-package :cl-user)
(defpackage corona-test-asd
  (:use :cl :asdf))
(in-package :corona-test-asd)

(eval-when (:execute :load-toplevel :compile-toplevel)
  (push :corona-testing *features*))

(defsystem corona-test
  :author "Fernando Borretti"
  :license "MIT"
  :depends-on (:corona
               :fiveam
               :clack
               :clack-v1-compat
               :archive
               :cl-fad)
  :components ((:module "t"
                :serial t
                :components
                ((:file "setup")
                 (:file "tests")))))
