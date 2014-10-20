(in-package :cl-user)
(defpackage corona-web.files
  (:use :cl))
(in-package :corona-web.files)

(defparameter +index+
  (asdf:system-relative-pathname :corona #p"index.html"))
(defparameter +stylesheet+
  (asdf:system-relative-pathname :corona #p"web/style.lass"))

(defun write-file (pathname content)
  (with-open-file (stream pathname
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (write-string content stream)))

(write-file +index+ (corona-web.tmpl:index))
(lass:generate +stylesheet+)
