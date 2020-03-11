(cl:in-package :cl-user)

(defpackage #:quickdist
  (:use #:cl #:alexandria)
  (:export #:quickdist

           #:*gnutar*
           #:make-dist-from-directory
           #:package-dist
           #:retry-loading-asd))
