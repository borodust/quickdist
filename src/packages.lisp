(cl:defpackage #:quickdist
  (:use :cl)
  (:export #:*gnutar*
           #:retry-loading-asd

           #:generate-metadata
           #:generate-archives
           #:quickdist))
