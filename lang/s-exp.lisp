(uiop:define-package :vernacular/lang/s-exp
    (:use)
  (:mix :vernacular/shadows :alexandria :serapeum)
  (:import-from :vernacular :with-meta-language :module-progn-in)
  (:export :read-module :module-progn))

(in-package :vernacular/lang/s-exp)

(defconst eof "eof")

(defun read-module (path stream)
  (with-meta-language (path stream)
    (declare (ignore path))
    (let ((*read-eval* nil))
      (loop for form = (cl:read stream nil eof)
            until (eq form eof)
            collect form))))

(defmacro module-progn (&rest forms)
  `(progn ,@forms))
