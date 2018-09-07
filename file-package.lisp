(defpackage :vernacular/file-package
  (:use :cl
    :alexandria :serapeum
    :uiop/pathname
    :overlord/types)
  (:import-from :vernacular/specials :*language*)
  (:import-from :overlord/base :current-system)
  (:import-from :overlord/asdf
    :primary-asdf-system-name
    :asdf-system-base)
  (:export
   :find-file-package
   :ensure-file-package
   :define-file-package
   :in-file-package))

(in-package :vernacular/file-package)

(deftype symbol-status ()
  '(member null :internal :external :inherited))

(defparameter *suffix-bytes* 4)

(defun ensure-file-package (file)
  (check-type file pathname-designator)
  (assure package
    (or (find-file-package file)
        (make-file-package file))))

(defun find-file-package (file)
  (let ((name (file-package-name file)))
    (find-package name)))

(defun make-file-package (file)
  (let ((name (file-package-name file)))
    (make-package name)))

(defun file-package-name (file)
  (let* ((file (pathname file))
         (system (current-system))
         (base (asdf-system-base system))
         (relative-path (enough-pathname file base))
         (system-name (primary-asdf-system-name system))
         (relative-dirs
           (drop-while #'keywordp
                       (pathname-directory relative-path))))
    (assert (relative-pathname-p file))
    (fmt "~a/~a/~a.~a"
         system-name
         (string-join relative-dirs "/")
         (pathname-name file)
         (or (pathname-type file) ""))))

(defmacro in-file-package (file)
  (let ((name (file-package-name file)))
    `(in-package ,name)))

(defmacro define-file-package (source)
  (let* ((package (ensure-file-package source))
         (name (package-name package)))
    `(defpackage ,name
       ;; Pass the use list of the existing package into the
       ;; macroexpansion.
       (:use ,(string *language*)))))
