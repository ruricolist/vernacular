(uiop:define-package :vernacular/simple-module
  (:use)
  (:documentation "Reference implementation of a module.
Most languages will expand into `simple-module' forms.")
  (:mix :serapeum :alexandria :vernacular/shadows :vernacular/types)
  (:import-from :alexandria :mappend)
  (:import-from :trivia)
  (:import-from :vernacular/types :vernacular-error)
  (:import-from :serapeum :op :car-safe :keep)
  (:import-from :vernacular/module
    :module-ref
    :module-ref-ns
    :module-exports)
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

(defcondition simple-module-error (vernacular-error)
  ((module :initarg :module)))

(defcondition ns-error (simple-module-error)
  ((ns :initarg :ns)))

(defcondition no-macros-please (simple-module-error)
  ()
  (:report (cl:lambda (c s) (declare (ignore c))
             (format s "Simple modules cannot export macros."))))

(defcondition not-exported (ns-error)
  ((name :initarg :name))
  (:report (cl:lambda (c s)
             (with-slots (module name) c
               (format s "~s not exported by module ~a."
                       name module)))))

(defun not-exported (module name ns)
  (error 'not-exported
         :module module
         :name name
         :ns ns))

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
         (export-forms (keep :export forms :key #'car)))
    (mappend #'rest export-forms)))



(defun export-expr (spec)
  (cond ((eql 'macro-function (public-ns spec))
         (error 'no-macros-please))
        ((eql 'setf (private-ns spec))
         `(function (setf ,(private-name spec))))
        (t (private-side spec))))

(defstruct-read-only simple-module
  (exports nil :type list)
  (exports-table (lambda (module key ns)
                   (error 'not-exported
                          :module module
                          :ns ns
                          :name key))
                 :type function))

(defmethod module-exports ((sm simple-module))
  (simple-module-exports sm))

(defmethod module-ref-ns ((sm simple-module) name (ns (eql 'macro-function)))
  (declare (ignore name))
  (error 'no-macros-please))

(defmethod module-ref-ns ((sm simple-module) name ns)
  (funcall (simple-module-exports-table sm) sm name ns))

(defmacro simple-module ((&rest exports) &body body)
  (let ((export-keys (nub (mapcar #'public-side exports))))
    `(make-simple-module
      :exports ',export-keys
      :exports-table (mlet ,exports
                       ,@body))))

(defmacro mlet (exports &body body)
  (setf exports (nub exports))
  (with-unique-names (simple-module-lookup)
    `(local*
       ,@body
       ;; The name for the lambda is just to make debugging easier.
       (named-lambda ,simple-module-lookup (module key ns)
         ,(let ((by-ns (assort exports :key #'public-ns)))
            `(case ns
               ,@(loop for group in by-ns
                       for ns = (public-ns (first group))
                       ;; Wrap the ns in a list to keep the expansion
                       ;; readable. (No sharp quote.)
                       collect `((,ns)
                                 (case key
                                   ,@(loop for export in group
                                           for key = (make-keyword (public-name export))
                                           collect `((,key) ,(export-expr export)))
                                   (otherwise
                                    (not-exported module key ns)))))
               (otherwise
                (not-exported module key ns))))))))
