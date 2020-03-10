(cl:defpackage #:quickdist
  (:use :cl :alexandria)
  (:export #:*gnutar*
           #:retry-loading-asd

           #:generate-metadata
           #:generate-archives
           #:quickdist))
