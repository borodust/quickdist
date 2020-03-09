(cl:in-package #:quickdist)


(defun render-distinfo-template (name version base-url)
  (mustache:render (asdf:system-relative-pathname :quickdist "template/distinfo.template")
                   `((:name . ,name)
                     (:version . ,version)
                     (:base-url . ,base-url))))
