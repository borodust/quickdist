(cl:in-package #:quickdist)

;;;
;;; GENERAL
;;;
(defun join-strings (separator &rest strings)
  (format nil (concatenate 'string "~{~A~^" separator "~}") strings))


(defun effective-mtime (path)
  (if (not (fad:directory-pathname-p path))
      (file-write-date path)
      (apply #'max 0 (mapcar #'effective-mtime (fad:list-directory path)))))


(defun md5sum (path)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-file :md5 path)))


(defun last-directory-component (path)
  (first (last (pathname-directory path))))


(defun unix-filename (path)
  (format nil "~a.~a" (pathname-name path) (pathname-type path)))


(defun unix-filename-relative-to (base path)
  (let ((base-name (uiop:native-namestring (truename base)))
        (path-name (uiop:native-namestring (truename path))))
    (subseq path-name (mismatch base-name path-name))))


(defun format-date (universal-time)
  (let* ((time (multiple-value-list (decode-universal-time universal-time)))
         (timestamp (reverse (subseq time 0 6))))
    (format nil "~{~2,'0d~}" timestamp)))


(defmacro with-temporary-directory ((path) &body body)
  `(let ((,path (fad:pathname-as-directory (uiop:with-temporary-file (:pathname path) path))))
     (ensure-directories-exist ,path)
     (unwind-protect
          (progn ,@body)
       (fad:delete-directory-and-files ,path))))


(defun file-size (path)
  (with-open-file (stream path :element-type '(unsigned-byte 8))
    (file-length stream)))


(defun file (base &rest pathnames)
  (apply #'fad:merge-pathnames-as-file (append
                                        (list (uiop:ensure-directory-pathname base))
                                        (loop for (path . rest) on pathnames
                                              for relative = (uiop:enough-pathname path "/")
                                              collect (if rest
                                                          (uiop:ensure-directory-pathname relative)
                                                          (fad:pathname-as-file relative))))))


(defun dir (base &rest pathnames)
  (flet ((ensure-relative-dir (dir)
           (uiop:ensure-directory-pathname (uiop:enough-pathname dir "/"))))
    (apply #'fad:merge-pathnames-as-directory (append
                                               (list (uiop:ensure-directory-pathname base))
                                               (mapcar #'ensure-relative-dir pathnames)))))


(defun cp (src dst &key recursive)
  (inferior-shell:run/nil `("cp" ,@(when recursive
                                     (list "-R"))
                                 "-f" ,(uiop:native-namestring src) ,(uiop:native-namestring dst))))


(defun copy-directory-if (predicate src dst)
  (let ((dst (dir dst)))
    (ensure-directories-exist dst)
    (flet ((%cp (src-path)
             (let* ((relative (uiop:enough-pathname (uiop:native-namestring src-path)
                                                    (uiop:native-namestring src))))
               (ensure-directories-exist (fad:pathname-directory-pathname
                                          (merge-pathnames relative dst)))
               (unless (fad:directory-pathname-p relative)
                 (cp src-path (file dst relative))))))
      (fad:walk-directory src #'%cp :directories :depth-first :test predicate :follow-symlinks nil))))

;;;
;;; ARCHIVES
;;;
(defparameter *gnutar* "/bin/tar"
  "Location of the GNU TAR program")


(defun tar-content-sha1 (path)
  (let ((octets (uiop:with-temporary-file (:pathname pathname)
                  (inferior-shell:run `(inferior-shell:pipe
                                        (,*gnutar* "-xOf"
                                                   ,(uiop:native-namestring path))
                                        ("echo" inferior-shell:>
                                                ,(uiop:native-namestring pathname))))
                  (alexandria:read-file-into-byte-vector pathname))))
    (ironclad:byte-array-to-hex-string
     (ironclad:digest-sequence :sha1 octets))))


(defun archive (destdir-path source-path)
  (let* ((distignore (read-distignore-predicate source-path))
         (mtime (format-date (effective-mtime source-path)))
         (name (format nil "~a-~a" (last-directory-component source-path) mtime))
         (out-path (make-pathname :name name :type "tgz" :defaults (truename destdir-path))))
    (with-temporary-directory (tmp-source-path)
      (copy-directory-if (complement distignore) source-path tmp-source-path)
      (inferior-shell:run (list *gnutar* "-C" (uiop:native-namestring tmp-source-path) "."
                                "-czf" (uiop:native-namestring out-path)
                                "--transform" (format nil "s#^.#~a#" name))
                          :output *standard-output* :error-output *error-output*))
    out-path))


;;;
;;; DISTIGNORE
;;;
(defun read-distignore-predicate (path)
  (if-let ((distignore-file (probe-file (file path ".distignore"))))
    (flet ((trim-string (string)
             (string-trim '(#\Tab #\Space #\Newline) string)))
      (let* ((regexes (split-sequence:split-sequence #\Newline
                                                     (read-file-into-string distignore-file)))
             (scanners (mapcar #'ppcre:create-scanner (mapcar #'trim-string regexes))))
        (lambda (string)
          (let ((path (uiop:native-namestring path))
                (string (uiop:native-namestring string)))
            (when (starts-with-subseq path string)
              (let ((subpath (enough-namestring string path)))
                (loop for scanner in scanners
                        thereis (ppcre:scan scanner subpath))))))))
    (constantly nil)))





;;;
;;; SYSTEMS
;;;
(defun find-system-files (path global-distignore &key black-alist)
  (flet ((system-name->filename (name) (concatenate 'string name ".asd")))
    (let ((system-files nil)
          (blacklisted-filenames (mapcar #'system-name->filename black-alist))
          (distignore (read-distignore-predicate path)))
      (flet ((add-system-file (path)
               (push path system-files))
             (asd-file-p (path)
               (and (string-equal "asd" (pathname-type path))
                    (not (find (file-namestring path) blacklisted-filenames
                               :test #'equalp))
                    (not (funcall distignore path))
                    (not (funcall global-distignore path)))))
        (fad:walk-directory path #'add-system-file :test #'asd-file-p))
      (sort system-files #'string< :key #'pathname-name))))


(defun asdf-dependency-name (form)
  (if (listp form)
      (asdf-dependency-name
       (case (first form)
         (:feature (third form))
         (:version (second form))
         (:require (second form))))
      (string-downcase (string form))))


(defun load-asd (asd-path)
  (tagbody begin
     (restart-case
         (asdf:load-asd asd-path)
       (retry-loading-asd () (go begin)))))


(defun retry-loading-asd ()
  (when-let ((restart (find-restart 'retry-loading-asd)))
    (invoke-restart restart)))


(defun get-systems (asd-path)
  (handler-bind ((warning #'muffle-warning))
    (load-asd asd-path)
    (let ((asd-dir (namestring
                    (fad:pathname-directory-pathname (fad:pathname-as-file asd-path)))))
      (flet ((same-directory (system-name)
               (when-let ((asd-file (asdf:system-source-file system-name)))
                 (alexandria:starts-with-subseq asd-dir (namestring asd-file))))
             (list-dependencies (system)
               (sort (mapcar #'asdf-dependency-name
                             (append (asdf:system-defsystem-depends-on system)
                                     (asdf:system-depends-on system)))
                     #'string-lessp)))
        (sort (loop for system-name in (remove-if (complement #'same-directory)
                                                  (asdf:registered-systems))
                    as system = (asdf:find-system system-name)
                    collect (list* (string-downcase (asdf:component-name system))
                                   (list-dependencies system)))
              #'string-lessp
              :key #'first)))))

;;;
;;; SERIALIZATION
;;;
(defgeneric serialize (object))
(defgeneric deserialize (class input))

(defun serialize-hash-table (hash)
  (loop for key being the hash-key of hash
          using (hash-value value)
        collect `(,key ,(serialize value))))


(defun deserialize-hash-table (data value-class &rest args &key &allow-other-keys)
  (let ((table (apply #'make-hash-table args)))
    (loop for (key value) in data
          do (setf (gethash key table) (deserialize value-class value)))
    table))


(defmethod deserialize (class input)
  (apply #'make-instance class input))
