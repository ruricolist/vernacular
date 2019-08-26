(uiop:define-package :vernacular/simple-module
  (:use)
  (:documentation "Reference implementation of a module.
Most languages will expand into `simple-module' forms.")
  (:mix :serapeum :alexandria :vernacular/shadows :vernacular/types)
  (:import-from :alexandria :mappend)
  (:import-from :trivia :match :ematch)
  (:import-from :vernacular/types :vernacular-error)
  (:import-from :serapeum :op :car-safe :keep)
  (:import-from :vernacular/module :basic-module)
  (:import-from :vernacular/parsers :slurp-stream :slurp-file)
  (:import-from :vernacular/importing :with-imports)
  (:shadow :read-module :module-progn)
  (:export
   :read-module :module-progn
   :simple-module
   :static-exports))

(defpackage :vernacular/simple-module-user
  (:use :vernacular/simple-module :vernacular/shadows))

(in-package :vernacular/simple-module)

(defun read-module (source stream)
  (declare (ignore source))
  `(module-progn
     ,@(slurp-stream stream)))

(defmacro module-progn (&body body)
  (let* ((export-forms (keep :export body :key #'car-safe))
         (exports (mappend #'rest export-forms))

         (import-forms (keep :import body :key #'car-safe))
         (import-specs (mapcar #'rest import-forms))

         (body (remove-if (lambda (form)
                            (or (member form export-forms)
                                (member form import-forms)))
                          body))

         (module-form
           `(simple-module ,exports
              ,@body)))
    (reduce (curry #'list 'with-imports)
            import-specs
            :initial-value module-form
            :from-end t)))

(defun static-exports (source)
  (let* ((forms (slurp-file source))
         (export-forms (keep :export forms :key #'car))
         (exports (mappend #'rest export-forms)))
    (mapcar #'export-keyword exports)))



(defun export-keyword (spec)
  (make-keyword
   (ematch spec
     ((or (list (ns 'macro-function _) :as _)
          (ns 'macro-function _))
      (error "Simple modules cannot export macros."))
     ((type symbol) spec)
     ((ns _ name) name)
     ((list _ :as (and alias (type symbol)))
      alias)
     ((list _ :as (ns _ alias))
      alias))))

(defun export-binding (spec)
  (ematch spec
    ((or (ns 'macro-function _)
         (list (ns 'macro-function _) :as _))
     (error "Simple modules cannot export macros."))
    ((type symbol) spec)
    ((ns _ _) spec)
    ((list b :as _) b)))

(defstruct (simple-module (:include basic-module)))

(defmacro simple-module ((&rest exports) &body body)
  (let ((export-keys (mapcar #'export-keyword exports)))
    `(make-simple-module
      :exports ',export-keys
      :exports-table (mlet ,exports
                       ,@body))))

(defmacro mlet (exports &body body)
  `(local*
     ,@body
     ;; The name for the lambda is just to make debugging easier.
     (named-lambda ,(string-gensym 'simple-module-lookup) (module key)
       (declare (ignore module))
       ,(mlet-get exports 'key))))

(defun mlet-get (exports key)
  (let* ((export-keys (mapcar #'export-keyword exports))
         (export-bindings (mapcar #'export-binding exports)))
    ;; No duplicate exports.
    (assert (length= export-keys (nub export-keys)))
    `(case ,key
       ,@(mapcar #'list export-keys export-bindings)
       (t (vernacular-error "~a is not exported in this module."
                            ,key)))))
