(in-package :cl-user)
(defpackage corona-web.files
  (:use :cl))
(in-package :corona-web.files)

(defparameter +style+
  (lass:compile-and-write
   `(body
     :font-family "Source Sans Pro"
     :width "60%"
     :margin "0 auto"
     :color "#222")
   `(header
     :margin-bottom "20px"
     :overflow "auto"
     (img
      :width "150px"
      :float "left"
      :padding-right "20px")
     (.title
      :padding-top "75px"
      :font-size "3.5em")
     (.tagline
      :padding-top "10px"))
   `(h1
     :margin-bottom "0")
   `(hr
     :margin "0"
     :border "1px solid #f4c142")))

(defparameter +index+
  (asdf:system-relative-pathname :corona #p"index.html"))
(defparameter +stylesheet+
  (asdf:system-relative-pathname :corona #p"web/style.css"))

(defun write-file (pathname content)
  (with-open-file (stream pathname
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (write-string content stream)))

(write-file +index+ (corona-web.tmpl:index))
(write-file +stylesheet+ +style+)
