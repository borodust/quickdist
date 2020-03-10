(cl:in-package #:quickdist)


(defun render-distinfo-template (name version base-url)
  (mustache:render* (asdf:system-relative-pathname :quickdist "template/distinfo.template")
                    `((:name . ,name)
                      (:version . ,version)
                      (:base-url . ,base-url))))


(defun render-releases-template (data)
  (mustache:render* (asdf:system-relative-pathname :quickdist "template/releases.template")
                   `((:releases . ,data))))


(defun render-systems-template (data)
  (mustache:render* (asdf:system-relative-pathname :quickdist "template/systems.template")
                    `((:systems . ,data))))
