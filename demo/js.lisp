(defpackage :vernacular/demo/js
  (:documentation "Wrap CL-JS for use as a Vernacular language, for instructional purposes.")
  (:use :cl :alexandria :serapeum :cl-js)
  (:nicknames :demo/js)
  (:import-from :overlord/base
    :ensure-absolute)
  (:import-from :vernacular
    :dynamic-require-as
    :module-ref
    :module-exports)
  (:import-from :overlord/util :coerce-case)
  (:import-from :local-time)
  (:import-from :parse-js :parse-js)
  (:export :read-module :module-progn))

;;; TODO Getting values from an env.

(in-package :vernacular/demo/js)

;;; First thing first -- we need to get an ugly hack out of the way.
;;; CL-JS actually inlines the environment in the expansion, which
;;; won't work (invalid memory access) when compiling to a FASL. So we
;;; redefine the function that generates the code for global lookups.
;;; We can use `load-time-value' in the expansion to keep it
;;; efficient.
(defun cl-js::expand-global-lookup (prop)
  `(cl-js::gcache-lookup (load-time-value (cons nil (cl-js::make-cache (cl-js::intern-prop ,prop)))) *env*))

;;; This is the global context we will inject into CL-JS to enable
;;; modules to require other modules. CL-JS doesn't support the ES6
;;; static import syntax, so for our purposes a module, from the
;;; perspective of another module, is just an object.

(defparameter *vernacular-lib*
  (lret ((lib (empty-lib "Vernacular")))
    (add-to-lib lib
                (.func "require" (spec)
                       (vernacular:dynamic-require-default
                        :vernacular/demo/js
                        (ensure-absolute (uiop:parse-unix-namestring spec)))))))

(defstruct-read-only
    (js-module
     (:constructor make-js-module
         (obj &aux (exports-table (module-object-exports-table obj))
                   (exports-list (hash-table-keys exports-table)))))
  "Wrap a JS module for import-export."
  (obj :type js-obj)
  (exports-table :type hash-table)
  (exports-list :type list))

(defmethod module-exports ((m js-module))
  (js-module-exports-list m))

(defmethod module-ref ((m js-module) key)
  (gethash key (js-module-exports-table m)))

;;; Keys are usually keywords, but non-keywords are used for special
;;; purposes. The symbol `vernacular:default' gets the module's
;;; default export, if there is one.

(defmethod module-ref ((m js-module) (key (eql 'vernacular:default)))
  (js-module-obj m))

(defun object-keys (object)
  "List the keys of a JavaScript object."
  (collecting
    (js-for-in object #'collect :shallow)))

;;; The calling conventions for JavaScript functions and Lisp
;;; functions are different.

(defun js-fun->lisp-fun (fun)
  "Wrap FUN, a JS function, for calling from Lisp."
  (lambda (&rest args)
    (apply #'cl-js::js-funcall fun args)))

(defun module-object-exports-table (object)
  (alist-hash-table
   (mapcar (lambda (key)
             (let* ((value (js-prop object key))
                    (key-string (coerce-case key))
                    (key (make-keyword key-string))
                    (fun (js-fun->lisp-fun value)))
               (cons key fun)))
           (object-keys object))
   :test 'eq))

(defun wrap-as-module (ast)
  "Wrap AST as a function taking a single argument, named exports, and
returning it."
  `(:function nil ("exports") (,@(second ast) (:return (:name "exports")))))

(defun read-module (source stream)
  (declare (ignore source))
  `(module-progn
     (cl-js::wrap-js
      ,(cl-js::translate-ast
        (wrap-as-module
         (parse-js:parse-js stream))))))

(defmacro module-progn (&body body)
  (with-gensyms (fun obj mod)
    `(with-js-env (*vernacular-lib*)
       (lret* ((,fun ,`(progn ,@body))
               (,obj (js-call ,fun *env* (js-obj)))
               (,mod (make-js-module ,obj)))))))
