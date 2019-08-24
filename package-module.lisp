(defpackage :vernacular/package-module
  (:documentation "Support packages as modules.")
  (:use :cl :alexandria :serapeum)
  (:shadow :find-external-symbol)
  (:import-from :overlord/types :error*)
  (:import-from :vernacular/types :export-spec :var-spec :function-spec :macro-spec :export-alias)
  (:import-from :vernacular/module :module-ref :module-exports :current-module-source)
  (:import-from :vernacular/specials :*source* :*module*)
  (:import-from :vernacular/file-package :ensure-file-package)
  (:export :hash-table-package-module))
(in-package :vernacular/package-module)

(defmethod module-exports ((m package))
  (mapcar #'make-keyword (package-exports m)))

(defun find-external-symbol (name package)
  (mvlet ((sym status (find-symbol (string name) package)))
    (unless (and sym (eql status :external))
      (error "Package ~a does not export ~s." package sym))
    sym))

(defun unbound-error (sym)
  (error "Symbol ~s is unbound." sym))

(defmethod module-ref ((m package) name)
  (module-ref-ns m name nil))

(defmethod module-ref-ns ((m package) name (ns null))
  (let ((sym (find-external-symbol name m)))
    (symbol-value sym)))

(defmethod module-ref-ns ((m package) name (ns (eql 'macro-function)))
  (let ((sym (find-external-symbol name m)))
    (or (macro-function sym)
        (error "Not bound as a macro: ~s" sym))))

(defmethod module-ref-ns ((m package) name (ns (eql 'function)))
  (let ((sym (find-external-symbol name m)))
    (symbol-function sym)))
