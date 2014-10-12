(in-package :cl-user)
(defpackage corona-web.tmpl
  (:use :cl :cl-markup)
  (:export :index))
(in-package :corona-web.tmpl)

(defun head ()
  (markup
   (:head
    (:meta :charset "utf-8")
    (:meta :http-equiv "X-UA-Compatible" :content "IE=edge")
    (:meta :name "viewport" :content "width=device-width, initial-scale=1")
    (:title "Corona")
    (:link :rel "stylesheet" :href "https://maxcdn.bootstrapcdn.com/bootstrap/3.2.0/css/bootstrap.min.css")
    (:link :rel "stylesheet" :href "https://maxcdn.bootstrapcdn.com/font-awesome/4.2.0/css/font-awesome.min.css")
    (:link :rel "stylesheet" :href "web/style.css"))))

(defun footer ()
  (markup
   (:footer "")))

(defmacro layout (&rest content)
  `(html5
    (raw (head))
    (:body
     ,@content
     (:script :src "https://ajax.googleapis.com/ajax/libs/jquery/1.11.1/jquery.min.js"
              "")
     (:script :src "https://maxcdn.bootstrapcdn.com/bootstrap/3.2.0/js/bootstrap.min.js"
              "")
     (:script :src "web/scripts.js"
              "")
     (raw (footer)))))

(defun description ()
  (markup
   (:section :id "desc"
     (:p
      "Corona is a library for building and controlling virtual machines. It's
      essentially a clone of "
      (:a :href "https://www.vagrantup.com/" "Vagrant")
      ", with the advantage that it's written in pure Common Lisp, and can be
      installed simply from Quicklisp.")
     (:p
      "Corona uses "
      (:a :href "https://vagrantcloud.com/" "Vagrant Cloud")
      " as a source of base images for the virtual machines, so you can get
      started with any system in minutes."))))

(defun use-cases ()
  (markup
   (:h1 "Use Cases")
   (:h2 "Development Environments")
   (:p "Corona can be used to create isolated, reproducible development
environments so you and your team can work on the same system.")
   (:p "No more 'works on my machine', no more difference between development
and production.")
   (:h2 "Testing")
   (:p "If you have a library that uses an external tool, like a database server
or something equally large, you can use Corona to set up a virtual machine and
install whatever dependencies you need, so the user doesn't actually have to run
anything on their computer.")
   (:p "Additionally, since you can set up multiple virtual machines with
different systems, you can use Corona to ensure your library works on most
operating systems. This is especially useful for testing compilers and similar
applications where portability is critical.")
   (:h2 "Building")
   (:p "You can use Corona as a build server: Fire up virtual machines of the
operating system you want to build on, set them up with everything you need, and
run the builds.")))

(defun humanize-system-name (name)
  (cond
    ((equal name "freebsd")
     "FreeBSD")
    ((equal name "openbsd")
     "OpenBSD")
    (t
     (string-capitalize name))))

(defun available-systems ()
  (markup
   (:h1 "Available Systems")
   (:div :class "row"
     (:div :class "col-md-6"
       (:div :id "system-list"
             :class "list-group"
         (loop for sys in corona.sys::*known-systems* collecting
           (let ((name (string-downcase (symbol-name (corona.sys:name sys))))
                 (version (corona.sys:version sys))
                 (arch (corona.sys:arch sys)))
             (markup
              (:a :href "#machine-def-header" :class "list-group-item"
                  :data-sys-name name
                  :data-sys-version version
                  :data-sys-arch arch
                  (format nil "~A, ~A, ~A-bit"
                          (humanize-system-name name)
                          version
                          arch)))))))
     (:div :class "col-md-6"
       (:h2 :id "machine-def-header"
            "Machine Definition")
       (:pre
        (:code :id "machine-definition"
         "(defmachine my-machine)"))))))

(defun faq ()
  (markup
   (:h1 "FAQ")
   (:h2 "Why not Vagrant?")
   (:p "The advantage over Vagrant is simply that it's written in Common Lisp
and, as such, available in Quicklisp without any external commands. This way,
the library can be included as a dependency and used without anyone having to
set up an external tool other than VirtualBox.")))

(defun index ()
  (layout
   (:header
    (:h1 :class "title" "Corona")
    (:div :class "tagline"
          "Build and manage virtual machines from Common Lisp."))
   (raw (description))
   (:section :id "use-cases"
             (raw (use-cases)))
   (:section :id "systems"
             (raw (available-systems)))
   (:section :id "faq"
             (raw (faq)))))
