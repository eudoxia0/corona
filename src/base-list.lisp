;;;; A list of default known base boxes
(in-package :corona.base)

(defparameter +ubuntu-box+
  (make-instance '<base-box>
                 :name :ubuntu
                 :version :14.04
                 :arch :amd64
                 :source-url "http://releases.ubuntu.com/14.04.1/ubuntu-14.04.1-server-amd64.iso"
                 :checksum-type :md5
                 :checksum "ca2531b8cd79ea5b778ede3a524779b9"))

(defparameter +base-box-list+
  (list +ubuntu-box+))
