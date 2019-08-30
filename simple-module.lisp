(uiop:define-package :vernacular/simple-module
  (:use)
  (:documentation "Reference implementation of a module.
Most languages will expand into `simple-module' forms.")
  (:mix :serapeum :alexandria :vernacular/shadows :vernacular/types)
  (:import-from :alexandria :mappend)
  (:import-from :trivia :match :ematch)
  (:import-from :vernacular/types :vernacular-error)
  (:import-from :serapeum :op :car-safe :keep)
  (:import-from :vernacular/module :basic-module :module-ref :module-ref-ns)
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

(defconst reserved-prefix '%)

(defcondition simple-module-error (vernacular-error)
  ())

(defcondition no-macros-please (simple-module-error)
  ()
  (:report (cl:lambda (c s) (declare (ignore c))
             (format s "Simple modules cannot export macros."))))

(defcondition reserved-prefix (simple-module-error)
  ((name :initarg :name))
  (:report (cl:lambda (c s)
             (with-slots (name) c
               (format s "Simple modules forbid exports starting with ~a."
                       reserved-prefix)))))

(defcondition not-exported (simple-module-error)
  ((name :initarg :name))
  (:report (cl:lambda (c s)
             (with-slots (name) c
               (format s "~s not exported by this module."
                       name)))))

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



(defun setter-name (name)
  (make-keyword
   (string+ reserved-prefix
            'set-
            name)))

(defun check-reserved-prefix (name)
  (when (string^= reserved-prefix name)
    (error 'reserved-prefix :name name))
  (values))

(defun export-keyword (spec)
  (make-keyword
   (multiple-value-bind (name setter?)
       (ematch spec
         ((or (list (ns 'macro-function _) :as _)
              (ns 'macro-function _))
          (error 'no-modules-please))
         ((type symbol) spec)
         ((function-spec 'setf name)
          (values name t))
         ((ns 'setf name)
          (values name t))
         ((ns _ name) name)
         ((list _ :as (and alias (type symbol)))
          alias)
         ((list _ :as (ns 'setf alias))
          (values alias t))
         ((list _ :as (ns _ alias))
          alias))
     (check-reserved-prefix name)
     (if setter?
         (setter-name name)
         name))))

(defun export-binding (spec)
  (ematch spec
    ((or (ns 'macro-function _)
         (list (ns 'macro-function _) :as _))
     (error 'no-modules-please))
    ((type symbol) spec)
    ((ns _ _) spec)
    ((list b :as _) b)))

(defstruct (simple-module (:include basic-module)))

(defmethod module-ref-ns ((sm simple-module) name (ns (eql 'macro-function)))
  (declare (ignore name))
  (error 'no-modules-please))

(defmethod module-ref-ns ((sm simple-module) name (ns (eql 'setf)))
  (module-ref sm (setter-name name)))

(defmacro simple-module ((&rest exports) &body body)
  (let ((export-keys (nub (mapcar #'export-keyword exports))))
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
  ;; No duplicate exports.
  (assert (length= exports (nub exports :test #'equal)))
  (let* ((export-keys (mapcar #'export-keyword exports))
         (export-bindings (mapcar #'export-binding exports)))
    `(case ,key
       ,@(mapcar #'list export-keys export-bindings)
       (t (error 'not-exported :name ,key)))))
