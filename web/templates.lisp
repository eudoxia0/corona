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
    (:link :rel "text/css" :href "http://fonts.googleapis.com/css?family=Source+Sans+Pro")
    (:link :rel "stylesheet" :href "web/style.css"))))

(defun header ()
  (markup
   (:header
    (:img :src "logo.jpg")
    (:div :class "title" "Corona")
    (:div :class "tagline"
          "Build and manage virtual machines from Common Lisp."))))

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
     (:script :src "web/scripts.js" "")
     (:script :src "web/highlight-lisp/highlight-lisp.js" "")
     (:link :rel "stylesheet" :href "web/highlight-lisp/themes/github.css")
     (raw (footer)))))

(defun description ()
  (markup
   (:h1 "What is this?")
   (:hr)
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
      started with any system in minutes.")
     (:p
      "Corona just manages the machines. To access them through SSH, consider the "
      (:code
       (:a :href "https://github.com/eudoxia0/trivial-ssh"
           "trivial-ssh"))
      " library.")
     (:p
      (:a :href "https://github.com/eudoxia0/corona"
          "View the source code on GitHub")
      "."))))

(defun use-cases ()
  (markup
   (:h1 "Use Cases")
   (:hr)
   (:section :id "use-cases"
     (:h2 "Development Environments")
     (:p "Corona can be used to create isolated, reproducible development
environments so you and your team can work on the same system.")
     (:p "No more 'works on my machine', no more difference between development
and production.")
     (:h2 "Testing")
     (:p "If you have a library that uses an external tool, like a database
server or something equally large, you can use Corona to set up a virtual
machine and install whatever dependencies you need, so the user doesn't actually
have to run anything on their computer.")
     (:p "Additionally, since you can set up multiple virtual machines with
different systems, you can use Corona to ensure your library works on most
operating systems. This is especially useful for testing compilers and similar
applications where portability is critical.")
     (:h2 "Building")
     (:p "You can use Corona as a build server: Fire up virtual machines of the
operating system you want to build on, set them up with everything you need, and
run the builds."))))

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
   (:hr)
   (:section :id "systems"
     (:ul :id "system-list"
       (:h2 "List of Systems")
       (loop for sys in corona.sys::*known-systems* collecting
         (let ((name (string-downcase (symbol-name (corona.sys:name sys))))
               (version (corona.sys:version sys))
               (arch (corona.sys:arch sys)))
           (markup
            (:li
             :data-sys-name name
             :data-sys-version version
             :data-sys-arch arch
             (:a :href "#machine-def-header"
                 (format nil "~A, ~A, ~A-bit"
                         (humanize-system-name name)
                         version
                         arch)))))))
     (:div :id "machine-definition"
           (:h2 :id "machine-def-header"
                "Machine Definition")
           (:span "Click on a system in the list to get the machine definition.")
           (:pre
            (:code :class "lisp"
                   :id "definition-code"
                   "(defmachine my-machine)"))))))

(setf 3bmd-code-blocks:*code-blocks* t
      3bmd-definition-lists:*definition-lists* t)

(defun parse-markdown (pathname)
  (with-output-to-string (str)
    (3bmd:parse-string-and-print-to-stream
     (uiop:read-file-string pathname)
     str)))

(defparameter +usage+
  (parse-markdown (asdf:system-relative-pathname :corona "USAGE.md")))

(defun usage ()
  (markup
   (:h1 "Usage")
   (:hr)
   (:section :id "usage"
     (raw +usage+))))

(defun faq ()
  (markup
   (:h1 "FAQ")
   (:hr)
   (:section :id "faq"
     (:h2 "Why not Vagrant?")
     (:p "The advantage over Vagrant is simply that it's written in Common Lisp
and, as such, available in Quicklisp without any external commands. This way,
the library can be included as a dependency and used without anyone having to
set up an external tool other than VirtualBox."))))

(defun index ()
  (layout
   (raw (header))
   (raw (description))
   (raw (use-cases))
   (raw (available-systems))
   (raw (usage))
   (raw (faq))))
