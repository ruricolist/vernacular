(defpackage :vernacular/hash-table-module
  (:documentation "Support hash tables as modules.")
  (:use :cl :alexandria :serapeum)
  (:import-from :vernacular/types :vernacular-error)
  (:import-from :vernacular/module :module-ref :module-exports)
  (:export :hash-table-module))
(in-package :vernacular/hash-table-module)

(defun hash-table-module (hash-table)
  "Wrap HASH-TABLE as a module."
  (lret ((module (copy-hash-table hash-table :test #'eq)))
    (unless (= (hash-table-count module)
               (hash-table-count hash-table))
      (error (vernacular-error "Hash table ~a contains duplicate keys.")))))

(defmethod module-ref ((m hash-table) name)
  (gethash name m))

(defmethod module-exports ((m hash-table))
  (hash-table-keys m))
