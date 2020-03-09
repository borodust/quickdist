(cl:in-package #:quickdist)

;;;
;;; RELEASE DESCRIPTOR
;;;
(defclass release-descriptor ()
  ((name :initarg :name)
   (last-modified :initarg :last-modified)
   (system-files :initarg :system-files)))


(defun make-release-descriptor (name last-modified system-files)
  (make-instance 'release-descriptor :name name
                                     :last-modified last-modified
                                     :system-files system-files))


(defmethod serialize ((this release-descriptor))
  (with-slots (name last-modified system-files) this
    `(:name ,name
      :last-modified ,last-modified
      :system-files ,system-files)))


;;;
;;; SYSTEM DESCRIPTOR
;;;
(defclass system-descriptor ()
  ((name :initarg :name)
   (project :initarg :project)
   (file :initarg :file)
   (dependencies :initarg :dependencies)))


(defun make-system-descriptor (project-name system-file system-name system-dependencies)
  (make-instance 'system-descriptor :name system-name
                                    :file system-file
                                    :dependencies system-dependencies
                                    :project project-name))


(defmethod serialize ((this system-descriptor))
  (with-slots (name project file dependencies) this
    `(:project ,project
      :name ,name
      :file ,file
      :dependencies ,dependencies)))


;;;
;;; DIST
;;;
(defclass dist ()
  ((name :initarg :name :reader %name-of)
   (version :initarg :version :reader %version-of)
   (base-url :initarg :base-url :reader %base-url-of)
   (release-index :initarg :release-index)
   (system-index :initarg :system-index)))


(defmethod serialize ((this dist))
  (with-slots (name version base-url release-index system-index) this
    `(:name ,name
      :version ,version
      :base-url ,base-url
      :release-index ,(serialize-hash-table release-index)
      :system-index ,(serialize-hash-table system-index))))


(defmethod deserialize ((class (eql 'dist)) data)
  (destructuring-bind (&key name version base-url release-index system-index) data
    (make-instance 'dist
                   :name name
                   :version version
                   :base-url base-url
                   :release-index (deserialize-hash-table release-index 'release-descriptor :test 'equal)
                   :system-index (deserialize-hash-table system-index 'system-descriptor :test 'equal))))


(defun make-dist (name base-url &key (version :today)
                                  release-index
                                  system-index)
  (make-instance 'dist :name name
                       :version version
                       :base-url base-url
                       :release-index release-index
                       :system-index system-index))


(defun register-release (index project-name release-path system-files)
  (with-simple-restart (skip-project "Skip this project, continue with the next.")
    (let ((release (make-release-descriptor project-name
                                            (effective-mtime release-path)
                                            (mapcar (curry #'unix-filename-relative-to
                                                           release-path)
                                                    system-files))))
      (setf (gethash project-name index) release))))


(defun register-systems (index project-name system-files)
  (dolist (system-file system-files)
    (dolist (name-and-dependencies (get-systems system-file))
      (let* ((*print-case* :downcase)
             (system-file (pathname-name system-file))
             (system-name (first name-and-dependencies))
             (system-dependencies (rest name-and-dependencies))
             (system (make-system-descriptor project-name
                                             system-file
                                             system-name
                                             system-dependencies)))
        (setf (gethash (list project-name system-name) index) system)))))


(defun make-dist-from-directory (name base-url project-path)
  (let* ((project-path (fad:pathname-as-directory project-path))
         (global-distignore (make-distignore-predicate project-path))
         (release-index (make-hash-table :test 'equal))
         (system-index (make-hash-table :test 'equal)))
    (dolist (release-path (fad:list-directory project-path))
      (when (fad:directory-pathname-p project-path)
        (let* ((project-name (last-directory-component release-path))
               (system-files (find-system-files release-path global-distignore)))
          (when system-files
            (with-simple-restart (skip-project "Skip this project, continue with the next.")
              (register-release release-index
                                project-name
                                release-path
                                system-files)
              (register-systems system-index project-name system-files))))))
    (make-dist name base-url
               :version (format-date (effective-mtime project-path))
               :release-index release-index
               :system-index system-index)))


(defun save-dist (dist pathname)
  (let ((pathname (fad:pathname-as-file pathname)))
    (ensure-directories-exist (fad:pathname-directory-pathname pathname))
    (with-open-file (out pathname :if-exists :supersede :direction :output)
      (prin1 (serialize dist) out))
    (values)))


(defun load-dist (pathname)
  (let ((pathname (fad:pathname-as-file pathname)))
    (with-open-file (in pathname :direction :input)
      (uiop:with-safe-io-syntax ()
        (let ((*read-eval* nil))
          (deserialize 'dist (read in)))))))


;; METADATA

(defun merge-dist-subscription-distinfo-filename (dist base-path)
  (fad:merge-pathnames-as-file (fad:pathname-as-directory base-path)
                               (format nil "~A.txt" (%name-of dist))))


(defun merge-distinfo-filename (dist base-path)
  (fad:merge-pathnames-as-file (fad:pathname-as-directory base-path)
                               (format nil "~A.txt" (%name-of dist))))


(defun merge-dist-version-directory (dist base-path)
  (fad:merge-pathnames-as-directory (fad:pathname-as-directory base-path)
                                    (%name-of dist)
                                    (%version-of dist)))

(defun merge-dist-archive-directory (dist base-path)
  (fad:merge-pathnames-as-directory (fad:pathname-as-directory base-path)
                                    (%name-of dist)
                                    "archive"))


(defun dist-archive-url (dist)
  (format nil "~A/~A/archive" (%base-url-of dist) (%name-of dist)))


(defun render-subscription-distinfo (dist base-path)
  (alexandria:write-string-into-file
   (render-distinfo-template (%name-of dist) (%version-of dist) (%base-url-of dist))
   (merge-distinfo-filename dist base-path)))

;; ARCHIVES
