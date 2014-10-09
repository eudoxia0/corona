(in-package :cl-user)
(defpackage corona-test-asd
  (:use :cl :asdf))
(in-package :corona-test-asd)

(push :corona-testing *features*)

(defsystem corona-test
  :author "Fernando Borretti"
  :license "MIT"
  :depends-on (:corona
               :fiveam
               :clack
               :archive)
  :components ((:module "t"
                :serial t
                :components
                ((:file "setup")
                 (:file "tests")))))
