(in-package :corona.sys)

;;;; Linux distributions

;;; Ubuntu

(define-system
  :name :ubuntu
  :version :14.04
  :arch :64
  :cloud-box (define-box
               :name "ubuntu-14.04"
               :author "chef"))

(define-system
  :name :ubuntu
  :version :14.04
  :arch :32
  :cloud-box (define-box
               :name "ubuntu-14.04-i386"
               :author "chef"))

(define-system
  :name :ubuntu
  :version :13.10
  :arch :64
  :cloud-box (define-box
               :name "ubuntu-13.10"
               :author "chef"))

(define-system
  :name :ubuntu
  :version :13.10
  :arch :32
  :cloud-box (define-box
               :name "ubuntu-13.10-i386"
               :author "chef"))

(define-system
  :name :ubuntu
  :version :13.04
  :arch :64
  :cloud-box (define-box
               :name "ubuntu-13.04"
               :author "chef"))

(define-system
  :name :ubuntu
  :version :13.04
  :arch :32
  :cloud-box (define-box
               :name "ubuntu-13.04-i386"
               :author "chef"))

(define-system
  :name :ubuntu
  :version :10.04
  :arch :64
  :cloud-box (define-box
               :name "ubuntu-10.04"
               :author "chef"))

(define-system
  :name :ubuntu
  :version :10.04
  :arch :32
  :cloud-box (define-box
               :name "ubuntu-10.04-i386"
               :author "chef"))

;;; Debian

(define-system
  :name :debian
  :version :7.6
  :arch :64
  :cloud-box (define-box
               :name "debian-7.6"
               :author "chef"))

(define-system
  :name :debian
  :version :7.6
  :arch :32
  :cloud-box (define-box
               :name "debian-7.6-i386"
               :author "chef"))

(define-system
  :name :debian
  :version :7.4
  :arch :64
  :cloud-box (define-box
               :name "debian-7.4"
               :author "chef"))

(define-system
  :name :debian
  :version :7.4
  :arch :32
  :cloud-box (define-box
               :name "debian-7.4-i386"
               :author "chef"))

;;; CentOS

(define-system
  :name :centos
  :version :6.5
  :arch :64
  :cloud-box (define-box
               :name "centos-6.5"
               :author "chef"))

(define-system
  :name :centos
  :version :6.5
  :arch :32
  :cloud-box (define-box
               :name "centos-6.5-i386"
               :author "chef"))

;;; Fedora

(define-system
  :name :fedora
  :version :20
  :arch :64
  :cloud-box (define-box
               :name "fedora-20"
               :author "chef"))

(define-system
  :name :fedora
  :version :20
  :arch :32
  :cloud-box (define-box
               :name "fedora-20-i386"
               :author "chef"))

(define-system
  :name :fedora
  :version :19
  :arch :64
  :cloud-box (define-box
               :name "fedora-19"
               :author "chef"))

;;;; BSD forks

;;; FreeBSD

(define-system
  :name :freebsd
  :version :10.0
  :arch :64
  :cloud-box (define-box
               :name "freebsd-10.0"
               :author "chef"))

(define-system
  :name :freebsd
  :version :9.2
  :arch :64
  :cloud-box (define-box
               :name "freebsd-9.2"
               :author "chef"))

(define-system
  :name :freebsd
  :version :9.2
  :arch :32
  :cloud-box (define-box
               :name "freebsd-9.2-i386"
               :author "chef"))

;;; OpenBSD

(define-system
  :name :openbsd
  :version :5.5
  :arch :64
  :cloud-box (define-box
               :name "openbsd-5.5"
               :author "tmatilai"))
