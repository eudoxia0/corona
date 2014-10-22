(in-package :cl-user)
(defpackage corona-web-asd
  (:use :cl :asdf))
(in-package :corona-web-asd)

(defsystem corona-web
  :author "Fernando Borretti"
  :license "MIT"
  :depends-on (:corona
               :cl-markup
               :lass
               :3bmd
               :3bmd-ext-code-blocks
               :3bmd-ext-definition-lists)
  :components ((:module "web"
                :serial t
                :components
                ((:file "templates")
                 (:file "files")))))
