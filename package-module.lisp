(uiop:define-package :vernacular/package-module
  (:documentation "Support packages as modules.")
  (:use)
  (:mix :vernacular/shadows :alexandria :serapeum)
  (:shadow :find-external-symbol)
  (:import-from :overlord/types :error*)
  (:import-from :vernacular/types :ns :function-spec)
  (:import-from :vernacular/module :module-ref :module-ref-ns :module-exports)
  (:import-from :vernacular/specials :*source* :*module*)
  (:import-from :vernacular/file-package :intern-file-package)
  (:import-from :vernacular/parsers
    :slurp-stream)
  (:shadowing-import-from :vernacular/importing
    :import)
  (:import-from :trivia :ematch :match)
  (:export :hash-table-package-module)
  (:shadow :read-module :module-progn)
  (:export :read-module :module-progn)
  (:reexport :vernacular/shadows))
(in-package :vernacular/package-module)

(defparameter *compile-top-level* t)

(def package (find-package :vernacular/package-module))

(defun read-module (source stream)
  (let ((package (intern-file-package source :use-list (list package))))
    `(module-progn
       ,@(slurp-stream stream
                       :package package))))

(defmacro module-progn (&body body)
  (let* ((export-forms (keep :export body :key #'car-safe))
         (exports (mappend #'rest export-forms))

         (import-forms (keep :import body :key #'car-safe))

         (body (remove-if (lambda (form)
                            (or (member form export-forms)
                                (member form import-forms)))
                          body)))
    (with-unique-names (source pkg)
      `(progn
         ,@(mapcar (op `(import ,@(rest _)))
                   import-forms)
         ,@body
         (let* ((,source ,*source*)
                (,pkg (intern-file-package ,source))
                (*package* ,pkg))
           (export
            ',(loop for export in exports
                    collect (ematch export
                              ((type symbol) export)
                              ((function-spec 'setf export)
                               export)
                              ((ns _ export) export)))
            ,pkg)
           (setq *module* ,pkg))))))

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

(defmethod module-ref-ns ((m package) name (ns (eql 'cl:macro-function)))
  (let ((sym (find-external-symbol name m)))
    (or (macro-function sym)
        (error "Not bound as a macro: ~s" sym))))

(defmethod module-ref-ns ((m package) name (ns (eql 'cl:function)))
  (let ((sym (find-external-symbol name m)))
    (symbol-function sym)))

(defmethod module-ref-ns ((m package) name (ns (eql 'cl:setf)))
  (let ((sym (find-external-symbol name m)))
    (let ((key-list (list 'cl:setf sym)))
      (declare (dynamic-extent key-list))
      (fdefinition key-list))))
