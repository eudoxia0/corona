(in-package :corona.sys)

;;;; Linux distributions

;;; Ubuntu

(define-system
  :name :ubuntu
  :version :xenial64
  :arch :64
  :box (define-box
           :name "xenial64"
           :author "ubuntu"
           :version "20210119.0.0"))

(define-system
  :name :ubuntu
  :version :trusty64
  :arch :64
  :box (define-box
           :name "trusty64"
           :author "ubuntu"
           :version "20190514.0.0"))

(define-system
  :name :ubuntu
  :version :trusty32
  :arch :32
  :box (define-box
           :name "trusty32"
           :author "ubuntu"
           :version "20190514.0.0"))

;;; Debian

(define-system
  :name :debian
  :version :buster64
  :arch :64
  :box (define-box
           :name "buster64"
           :author "debian"
           :version "10.4.0"))

(define-system
  :name :debian
  :version :stretch64
  :arch :64
  :box (define-box
           :name "stretch64"
           :author "debian"
           :version "9.12.0"))

(define-system
  :name :debian
  :version :jessie64
  :arch :64
  :box (define-box
           :name "jessie64"
           :author "debian"
           :version "8.11.1"))

;;; CentOS

(define-system
  :name :centos
  :version :7
  :arch :64
  :box (define-box
               :name "7"
               :author "centos"
               :version "2004.01"))

;;; Fedora

(define-system
  :name :fedora
  :version :25
  :arch :64
  :box (define-box
           :name "25-cloud-base"
           :author "fedora"
           :version "20161122"))


(define-system
  :name :fedora
  :version :24
  :arch :64
  :box (define-box
           :name "24-cloud-base"
           :author "fedora"
           :version "20160621"))

;;;; BSD forks

;;; FreeBSD

(define-system
  :name :freebsd
  :version :12.2
  :arch :64
  :box (define-box
           :name "FreeBSD-12.2-RELEASE"
           :author "freebsd"
           :version "2020.10.23"))

(define-system
  :name :freebsd
  :version :11.4
  :arch :64
  :box (define-box
           :name "FreeBSD-11.4-RELEASE"
           :author "freebsd"
           :version "2020.06.12"))


(define-system
  :name :freebsd
  :version :10.4
  :arch :64
  :box (define-box
           :name "FreeBSD-10.4-RELEASE"
           :author "freebsd"
           :version "2017.09.29"))
;;; OpenBSD

(define-system
  :name :openbsd
  :version :5.5
  :arch :64
  :box (define-box
           :name "openbsd-5.5"
           :author "tmatilai"
           :version "1.0.0"))

;; Simulate keeping the order
(setf *known-systems* (reverse *known-systems*))
