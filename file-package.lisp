(defpackage :vernacular/file-package
  (:documentation "Generate a unique per-file package.")
  (:use :cl
    :alexandria :serapeum
    :uiop/pathname
    :overlord/types)
  (:import-from :vernacular/specials :*language*)
  (:import-from :overlord/base :current-system)
  (:import-from :overlord/digest :digest-string)
  (:import-from :overlord/asdf
    :asdf-system-name-keyword
    :asdf-system-name
    :asdf-system-base)
  (:import-from :s-base64 :encode-base64-bytes)
  (:export
   :find-file-package
   :ensure-file-package
   :reset-file-package))

(in-package :vernacular/file-package)

(deftype symbol-status ()
  '(member null :internal :external :inherited))

(defun ensure-file-package (file &key use-list)
  (check-type file pathname-designator)
  (check-type use-list (list-of package-designator))
  (assure package
    (or (find-file-package file)
        (make-file-package file :use-list use-list))))

(defun reset-file-package (file &key use-list)
  (check-type file pathname-designator)
  (check-type use-list (list-of package-designator))
  (assure package
    (reset-package
     (ensure-file-package file :use-list use-list))))

(defun find-file-package (file &key use-list)
  (declare (ignore use-list))
  (let ((name (file-package-name file)))
    (find-package name)))

(defun make-file-package (file &key use-list)
  (let ((name (file-package-name file)))
    (make-package name :use use-list)))

(defun file-package-name (file)
  (let* ((system (current-system))
         (base (asdf-system-base system)))
    (assert (subpathp file base))
    (let ((namestring (enough-namestring file base))
          (system-name (asdf-system-name system)))
      (fmt "~(~a~).~a" system-name namestring))))

(defun make-suffix (suffix)
  (~>> suffix
       bytes->base64
       (string-right-trim "=")))

(defun bytes->base64 (bytes)
  (with-output-to-string (s)
    (encode-base64-bytes bytes s)))

(defun byte-array->string (byte-array &optional (base 16))
  (mapconcat (op (fmt "~(~vr~)" base _)) byte-array ""))



(defun reset-package (package)
  (assure package
    (reset-package/unintern-all package)))

;;; TODO It's not clear to me which of these is the best way to do it.

(defun reset-package/delete-and-recreate (pkg)
  (let ((name (package-name pkg))
        (use-list (package-use-list pkg))
        (nicknames (package-nicknames pkg)))
    (delete-package pkg)
    (make-package name
                  :use use-list
                  :nicknames nicknames)))

(defun reset-package/unintern-all (pkg)
  (dolist (sym (package-own-symbols pkg) pkg)
    (unintern sym pkg)))

(defun reset-package/undefine (pkg)
  (dolist (sym (package-own-symbols pkg) pkg)
    (cond-every
      ((fboundp sym) (fmakunbound sym))
      ;; Can't undo a special declaration.
      ((boundp sym) (unintern sym pkg))
      ;; Can't undo a symbol macro declaration.
      ((symbol-macro? sym) (unintern sym))
      ((class-name? sym) (setf (find-class sym) nil)))))

(defun symbol-macro? (sym)
  (and (symbolp sym)
       (not (eql sym
                 (macroexpand sym)))))

(defun class-name? (sym)
  (and (symbolp sym)
       (find-class sym :errorp nil)))

(defun package-own-symbols (pkg)
  (loop for sym being the present-symbols in pkg
        when (eql (symbol-package sym) pkg)
          collect sym))

(defun symbol-status (sym &optional (package (symbol-package sym)))
  (assure symbol-status
    (let ((name (symbol-name sym)))
      (nth-value 1
        (find-symbol name package)))))

(defun unintern-from-home-package (sym)
  (prog1 sym
    (when-let (package (symbol-package sym))
      (unintern sym package))))
