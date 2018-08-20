(uiop:define-package :vernacular/lang/sweet-exp
    (:mix :cl :vernacular/shadows :alexandria :serapeum)
  (:import-from :vernacular :with-meta-language :module-progn-in)
  (:import-from :readable :enable-sweet-real)
  (:export :read-module :module-progn))

(in-package :vernacular/lang/sweet-exp)

(defconst eof "eof")

(defun make-sweet-exp-readtable ()
  (let ((*readtable* (copy-readtable)))
    (enable-sweet-real)
    *readtable*))

(defun read-module (path stream)
  (with-meta-language (path stream)
    (declare (ignore path))
    (let ((*read-eval* nil)
          (*readtable* (make-sweet-exp-readtable)))
      (loop for form = (cl:read stream nil eof)
            until (eq form eof)
            collect form))))

(defmacro module-progn (&body forms)
  `(progn ,@forms))
