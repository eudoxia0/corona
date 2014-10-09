(in-package :corona-test)

;;; Test suites

(def-suite corona)
(in-suite corona)

(test testing-environment
  (is-true
   #+corona-testing t #-corona-testing nil)
  (is (equal corona.files:+corona-directory+
             (asdf:system-relative-pathname :corona #p"t/corona-files/"))))

(test start-box-server
  (finishes
    (setf *server-handler*
          (clack:clackup +server+ :port +server-port+))))

(test create-fake-files
  (finishes
    (loop for file in +tarball-file-list+ do
      (make-fake-file file)))
  (finishes
    (make-tarball +tarball-path+
                  +tarball-file-list+)))

(test stop-box-server
  (finishes
    (clack:stop *server-handler*)))

(run! 'corona)

(remove :corona-testing *features*)
