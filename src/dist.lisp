(cl:in-package #:quickdist)

;;;
;;; RELEASE DESCRIPTOR
;;;
(defclass release-descriptor ()
  ((name :initarg :name :reader %name-of)
   (last-modified :initarg :last-modified :reader %last-modified-of)
   (system-files :initarg :system-files :reader %system-files-of)))


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
  ((name :initarg :name :reader %name-of)
   (project :initarg :project :reader %project-of)
   (file :initarg :file :reader %file-of)
   (dependencies :initarg :dependencies :reader %dependencies-of)))


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
;;; ARCHIVE DESCRIPTOR
;;;
(defclass archive-descriptor ()
  ((project :initarg :project :reader %project-of)
   (name :initarg :name :reader %name-of)
   (file-md5 :initarg :md5 :reader %file-md5-of)
   (content-sha1 :initarg :sha1 :reader %content-sha1-of)
   (size :initarg :size :reader %size-of)))


(defun make-archive (project name file-md5 content-sha1 size)
  (make-instance 'archive-descriptor :project project
                                     :name name
                                     :md5 file-md5
                                     :sha1 content-sha1
                                     :size size))
;;;
;;; DIST
;;;
(defclass dist ()
  ((name :initarg :name :reader %name-of)
   (version :initarg :version :reader %version-of)
   (base-url :initarg :base-url :reader %base-url-of)
   (release-index :initarg :release-index :reader %release-index-of)
   (system-index :initarg :system-index :reader %system-index-of)))


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


(defun make-dist-from-directory (name base-url project-path &key version)
  (let* ((project-path (fad:pathname-as-directory project-path))
         (global-distignore (read-distignore-predicate project-path))
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
               :version (if (or (null version) (eq version :latest))
                            (format-date (effective-mtime project-path))
                            (format nil "~A" version))
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
(defun merge-distinfo-filename (dist base-path)
  (file base-path (format nil "~A.txt" (%name-of dist))))


(defun merge-dist-version-directory (dist base-path)
  (dir base-path (%name-of dist) (%version-of dist)))


(defun merge-dist-archive-directory (dist base-path)
  (dir base-path (%name-of dist) "archive"))


(defun dist-archive-url (dist archive-name)
  (format nil "~A/~A/archive/~A.tgz" (%base-url-of dist) (%name-of dist) archive-name))


(defun %render-distinfo (dist target-file)
  (alexandria:write-string-into-file
   (render-distinfo-template (%name-of dist) (%version-of dist) (%base-url-of dist))
   target-file)
  (values))


(defun render-distinfo (dist base-path)
  (let ((target-file (file (merge-dist-version-directory dist base-path) "distinfo.txt")))
    (ensure-directories-exist (fad:pathname-directory-pathname target-file))
    (%render-distinfo dist target-file)))


(defun render-subscription (dist base-path)
  (let ((target-file (merge-distinfo-filename dist base-path)))
    (ensure-directories-exist (fad:pathname-directory-pathname target-file))
    (%render-distinfo dist target-file)))


(defun render-releases (dist archive-index base-path)
  (flet ((get-archive (project)
           (if-let ((archive (gethash project archive-index)))
             archive
             (error "Archive for ~A missing" project))))
    (let ((data (loop for key being the hash-key of (%release-index-of dist) using (hash-value value)
                      for archive = (get-archive (%name-of value))
                      collect `((:name . ,(%name-of value))
                                (:url . ,(dist-archive-url dist (%name-of archive)))
                                (:archive-size . ,(%size-of archive))
                                (:archive-file-md5 . ,(%file-md5-of archive))
                                (:archive-content-sha1 . ,(%content-sha1-of archive))
                                (:archive . ,(%name-of archive))
                                (:system-files . ,(apply #'join-strings " " (%system-files-of value))))))
          (version-dir (merge-dist-version-directory dist base-path)))
      (ensure-directories-exist version-dir)
      (alexandria:write-string-into-file
       (render-releases-template data)
       (file version-dir "releases.txt"))))
  (values))


(defun render-systems (dist base-path)
  (let ((data (loop for key being the hash-key of (%system-index-of dist) using (hash-value value)
                    collect `((:project . ,(%project-of value))
                              (:file . ,(%file-of value))
                              (:name . ,(%name-of value))
                              (:dependencies . ,(if-let ((deps (%dependencies-of value)))
                                                  (format nil " ~A"
                                                          (apply #'join-strings " " deps))
                                                  "")))))
        (version-dir (merge-dist-version-directory dist base-path)))
    (ensure-directories-exist version-dir)
    (alexandria:write-string-into-file
     (render-systems-template data)
     (file version-dir "systems.txt")))
  (values))


(defun prepare-archive (release-path target-dir)
  (let* ((project-name (last-directory-component release-path))
         (archive-path (archive target-dir release-path))
         (archive-name (pathname-name archive-path))
         (file-size (file-size archive-path))
         (md5 (md5sum archive-path))
         (sha1 (tar-content-sha1 archive-path)))
    (make-archive project-name archive-name md5 sha1 file-size)))


(defun prepare-archives (dist project-dir base-path)
  (let ((target-dir (merge-dist-archive-directory dist base-path)))
    (ensure-directories-exist target-dir)
    (loop with archive-index = (make-hash-table :test 'equal)
          for name being the hash-key of (%release-index-of dist)
          for archive = (prepare-archive (dir project-dir name) target-dir)
          do (setf (gethash name archive-index) archive)
          finally (return archive-index))))


(defun package-dist (dist project-dir dist-dir)
  (with-temporary-directory (tmp-dir)
    (let* ((archive-index (prepare-archives dist project-dir tmp-dir)))
      (render-subscription dist tmp-dir)
      (render-distinfo dist tmp-dir)
      (render-releases dist archive-index tmp-dir)
      (render-systems dist tmp-dir))
    (ensure-directories-exist (dir dist-dir))
    (cp tmp-dir dist-dir :recursive t)))

;;;
;;; QUICKDIST COMPAT
;;;
(defun quickdist (&key (name "dist") (version :latest) (base-url "http://localhost/")
                    projects-dir dists-dir)
  (assert projects-dir)
  (let* ((dists-dir (or dists-dir (dir projects-dir "dist")))
         (dist (make-dist-from-directory name base-url projects-dir :version version)))
    (package-dist dist projects-dir dists-dir)))
