(defpackage :vernacular/importing
  (:use :cl :alexandria :serapeum
    :overlord/util
    :overlord/redo
    :overlord/global-state
    :overlord/base
    :overlord/target
    :uiop/filesystem
    :uiop/pathname

    :vernacular/module
    :vernacular/import-set
    :vernacular/types
    :vernacular/specials
    :vernacular/lang)
  (:import-from :overlord/types
    :error*
    :absolute-pathname)
  (:import-from :overlord/freeze
    :*before-hard-freeze-hook*)
  (:import-from :vernacular/types
    :vernacular-error)
  (:import-from :vernacular/shadows)
  (:shadow :import)
  (:export
   :import :import/local
   :import-default
   :import-as-package
   :import-as-subpackage
   :with-imports
   :with-import-default))
(in-package :vernacular/importing)

;;; Importing.

;;; Note that the import macros defined here expand into definition
;;; forms from vernacular/cl rather than from cl proper. (E.g.
;;; `vernacular/cl:defun' rather than `cl:defun'.) This is so
;;; languages that need to handle imports specially (e.g. Core Lisp)
;;; can do so simply by shadowing the relevant definition forms with
;;; `macrolet', instead of having to re-implement everything.

(defun expand-binding-spec (spec lang source)
  (setf source (merge-pathnames source (base))
        lang (lang-name lang))
  (flet ((get-static-exports ()
           ;; This doesn't save any work. The static bindings are
           ;; always computed every time we import from a module. But
           ;; we still only want to compute them here if we absolutely
           ;; have to. Why? For friendlier debugging. Doing the check
           ;; here would prevent us from macroexpanding `import' at
           ;; all if there was a problem with the imports, which is
           ;; frustrating. Instead, we push the check down into the
           ;; `check-static-bindings-now' macro.
           (receive (exports exports?)
               (module-static-exports lang source)
             (if exports? exports
                 (module-dynamic-exports lang source)))))
    (etypecase-of binding-spec spec
      ((eql :all)
       (loop for export in (get-static-exports)
             for sym = (intern (string export))
             collect `(,export :as ,sym)))
      ((eql :all-as-functions)
       (loop for export in (get-static-exports)
             for sym = (intern (string export))
             collect `(,export :as #',sym)))
      ((tuple :import-set list)
       (let ((import-set (second spec)))
         (expand-import-set import-set #'get-static-exports)))
      (list spec))))

(defmacro function-wrapper (fn)
  "Global definition for possible shadowing."
  fn)

(define-global-state *claimed-module-names* (make-hash-table :size 1024)
  "Table to track claimed modules, so we can warn if they are
  redefined.")

(defun claim-module-name (module source)
  "Warn if MODULE is already bound to a different LANG."
  (synchronized ()
    (let* ((table *claimed-module-names*)
           (old-value (gethash module table)))
      (when old-value
        (unless (equal old-value source)
          (warn "~s was claimed for ~a" module source)))
      (setf (gethash module table) source))))

(defun clear-claimed-module-names ()
  (clrhash (symbol-value '*claimed-module-names*)))

(add-hook '*before-hard-freeze-hook* 'hard-freeze-modules)

(defun lang+source (lang source module base &optional env)
  (setf source (macroexpand source env)) ;Allow a symbol macro as the source.
  (flet ((resolve-source (source)
           (merge-pathnames* (ensure-pathname source :want-pathname t)
                             base)))
    (econd
      ;; We have the source and the language.
      ((and source lang)
       (values (resolve-lang lang)
               (resolve-source source)))
      ;; We have the source, but not the language.
      ((and source (no lang))
       (let ((source (resolve-source source)))
         (values (resolve-lang
                  (or (guess-lang+pos source)
                      (required-argument :as)))
                 source)))
      ;; We have the language, but not the source.
      ((and lang (no source))
       (values (resolve-lang lang)
               (resolve-source
                (or (guess-source lang module)
                    (required-argument :from)))))
      ;; We have neither the language nor the source.
      ((nor lang source)
       (whichever
        (required-argument :as)
        (required-argument :from))))))

(defun resolve-import-spec
    (&key lang source bindings module (base (base)) env prefix)
  (check-type base absolute-pathname)
  (check-type prefix string-designator)
  (mvlet* ((lang source (lang+source lang source module base env))
           (bindings (expand-bindings bindings
                                      :lang lang
                                      :source source
                                      :prefix prefix)))
    (values lang source bindings)))

(defmacro import (module &body (&key
                                  ((:as lang))
                                  ((:from source))
                                  ((:binding bindings))
                                  prefix
                                  function-wrapper
                                  export-bindings)
                  &environment env)
  "Syntax for importing from modules."
  ;; Ensure we have both the lang and the source.
  (receive (lang source bindings)
      (resolve-import-spec :lang lang
                           :source source
                           :module module
                           :bindings bindings
                           :prefix prefix
                           :env env)
    ;; Warn if MODULE is already in use with another file.
    (claim-module-name module source)
    `(progn
       (import-module ,module
         :as ,lang
         :from ,(merge-pathnames* source (base)))
       ;; We push the check down into a separate macro so we can
       ;; inspect the overall macroexpansion without side effects.
       (check-static-bindings-now ,lang ,source ,bindings)
       (macrolet ((function-wrapper (fn)
                    ,(if function-wrapper
                         `(list ',function-wrapper fn)
                         'fn)))
         (import-bindings ,module
           ,@bindings))
       ;; BUG The function wrapper needs to be propagated into the
       ;; update task.
       (import-task ,module
         :as ,lang :from ,source
         :values ,bindings)
       ;; Fetch the symbols from bindings and export them.
       ,@(when export-bindings
           (let ((symbols (mapcar (compose #'second #'second) bindings)))
             `((export ',symbols))))
       ;; Strictly for debuggability.
       (values ',module ',bindings))))

(defun expand-bindings (bindings &key lang source prefix)
  ;; Avoid redundant calls to module-static-bindings.
  (~> bindings
      (expand-binding-spec lang source)
      canonicalize-bindings
      (apply-prefix prefix)))

(defmacro check-static-bindings-now (lang source bindings)
  "Wrapper around check-static-bindings to force evaluation at compile time.
Can't use eval-when because it has to work for local bindings."
  (check-static-bindings lang source bindings))

(defcondition binding-export-mismatch (vernacular-error)
  ((source :initarg :source)
   (bindings :initarg :bindings :type list)
   (exports :initarg :exports :type list))
  (:report (lambda (c s)
             (with-slots (bindings exports source) c
               (format s "Requested bindings do not match exports.~%Source: ~a~%Bindings: ~s~%Exports: ~s"
                       source bindings exports)))))

(defun check-static-bindings (lang source bindings)
  "Check that BINDINGS is free of duplicates. Also, using
`module-static-exports', check that all of the symbols being bound are
actually exported by the module specified by LANG and SOURCE."
  (ensure-lang-exists lang)
  (when bindings
    (check-static-bindings-1
     (ensure-lang-exists lang)
     (if (relative-pathname-p source)
         (merge-pathnames* source (base))
         source)
     (mapcar (op (import-keyword (first _)))
             (canonicalize-bindings bindings)))))

(defun check-exports (source bindings exports)
  "Make sure the bindings are a subset of the exports."
  (unless (subsetp bindings exports :test #'string=)
    (error 'binding-export-mismatch
           :source source
           :bindings bindings
           :exports exports)))

(defun check-static-bindings-1 (lang source bindings)
  (check-type lang keyword)
  (check-type source absolute-pathname)
  ;; (check-type bindings (satisfies setp))
  (unless (setp bindings)
    (error* "Duplicated bindings in ~a" bindings))
  (receive (static-exports exports-statically-known?)
      (module-static-exports lang source)
    (if exports-statically-known?
        (check-exports source bindings static-exports)
        (restart-case
            (let ((exports (module-dynamic-exports lang source)))
              (check-exports source bindings exports))
          (recompile-object-file ()
            :report "Recompile the object file."
            (let ((object-file (faslize lang source))
                  (target (module-spec lang source)))
              (delete-file-if-exists object-file)
              (build target)
              (check-static-bindings lang source bindings)))))))

(defmacro declaim-module (as from)
  `(propagate-side-effect
     (ensure-target-recorded
      (module-spec ,as ,from))))

(defmacro import-module (module &body (&key as from once))
  "When ONCE is non-nil, the module will only be rebuilt if it has not
yet been loaded."
  (check-type module var-alias)
  (let ((req-form
          (if once
              `(require-once ',as ,from)
              `(require-as ',as ,from))))
    `(progn
       (vernacular/shadows:def ,module ,req-form)
       (declaim-module ,as ,from)
       ',module)))

(defmacro import-default (var &body (&key as from))
  (let ((module-name (symbolicate '__module-for- var)))
    `(import ,module-name
       :as ,as
       :from ,from
       :binding ((:default :as ,var)))))

(defmacro import-task (module &body (&key as from values))
  (let ((task-name
          (etypecase-of import-alias module
            (var-alias module)
            ((or function-alias macro-alias)
             (second module)))))
    `(define-target-task ,task-name
       (setf ,module (require-as ',as ,from))
       (update-value-bindings ,module ,@values))))

(defmacro update-value-bindings (module &body values)
  `(progn
     ,@(collecting
         (dolist (clause values)
           (receive (import alias ref) (import+alias+ref clause module)
             (declare (ignore import))
             (collect
                 (etypecase-of import-alias alias
                   (var-alias `(setf ,alias ,ref))
                   (function-alias
                    `(setf (symbol-function ',(second alias)) ,ref))
                   ;; Do nothing. Macros cannot be imported as values.
                   (macro-alias nil))))))))

(defmacro import-bindings (module &body values)
  `(progn
     ,@(mapcar (op (import-binding _ module)) values)))

(defun canonicalize-binding (clause)
  (assure canonical-binding
    (if (typep clause 'canonical-binding)
        clause
        (etypecase-of binding-designator clause
          (var-spec
           (list (make-keyword clause) clause))
          (function-alias
           (list (make-keyword (second clause)) clause))
          (macro-alias
           (list (make-keyword (second clause)) clause))
          ((tuple symbol :as import-alias)
           (destructuring-bind (import &key ((:as alias))) clause
             (list (make-keyword import) alias)))))))

(defun canonicalize-bindings (clauses)
  (mapcar #'canonicalize-binding clauses))

(defun apply-prefix (clauses prefix)
  (if (null prefix) clauses
      (flet ((prefix (suffix) (symbolicate prefix suffix)))
        (loop for (import alias) in clauses
              collect (list import
                            (etypecase-of import-alias alias
                              (var-alias (prefix alias))
                              (function-alias `(function ,(prefix (second alias))))
                              (macro-alias `(macro-function ,(prefix (second alias))))))))))

(defun import-binding (clause module)
  (receive (import alias ref) (import+alias+ref clause module)
    (declare (ignore import))
    (etypecase-of import-alias alias
      (var-alias
       `(vernacular/shadows:def ,alias ,ref))
      (function-alias
       (let ((alias (second alias)))
         `(vernacular/shadows:defalias ,alias
            (assure function (function-wrapper ,ref)))))
      (macro-alias
       ;; Macros cannot be imported as values.
       (let ((alias (second alias)))
         (with-gensyms (whole body env)
           `(vernacular/shadows:defmacro ,alias (&whole ,whole &body ,body &environment ,env)
              (declare (ignore ,body))
              (funcall ,ref ,whole ,env))))))))

(defun import+alias+ref (clause module)
  (destructuring-bind (import alias) (canonicalize-binding clause)
    (let* ((key (import-keyword import))
           (ref
             (etypecase-of import-alias alias
               (var-alias `(module-ref/inline-cache ,module ',key))
               ((or function-alias macro-alias)
                `(module-fn-ref/inline-cache ,module ',key)))))
      (values import alias ref))))

(defun import-keyword (import)
  (if (symbolp import)
      (make-keyword import)
      (make-keyword (second import))))

(defmacro import/local (mod &body (&key from as binding prefix (once t))
                        &environment env)
  (receive (lang source bindings)
      (resolve-import-spec :lang as
                           :source from
                           :prefix prefix
                           :module mod
                           :bindings binding
                           :env env)
    ;; TODO If we knew that no macros were being imported, we could
    ;; give the module a local binding and not have to look it up
    ;; every time.
    `(progn
       (import-module ,mod :as ,lang :from ,source :once ,once)
       (check-static-bindings-now ,lang ,source ,bindings)
       (import-bindings ,mod ,@bindings))))

(defmacro with-imports ((mod &key from as binding prefix (once t)) &body body)
  "A version of `import' with local scope."
  `(local*
     (import/local ,mod
       :from ,from
       :as ,as
       :binding ,binding
       :prefix ,prefix
       :once ,once)
     (progn ,@body)))

(defmacro with-import-default ((bind &key from as (once t)) &body body)
  (with-unique-names (mod)
    `(with-imports (,mod
                    :from ,from
                    :as ,as
                    :once ,once
                    :binding ((:default :as ,bind)))
       ,@body)))

(defmacro import-as-package (package-name
                             &body body
                             &key ((:as lang))
                                  ((:from source) (guess-source lang package-name))
                                  ((:binding bindings))
                                  prefix
                             &allow-other-keys
                             &environment env)
  "Like `import', but instead of creating bindings in the current
package, create a new package named PACKAGE-NAME which exports all of
the symbols bound in the body of the import form."
  (receive (lang source bindings)
      (resolve-import-spec :lang lang
                           :source source
                           :bindings bindings
                           :module 'package-module
                           :prefix prefix
                           :env env)
    (declare (ignore source lang))
    (let ((body (list* :binding bindings
                       (remove-from-plist body :prefix :binding))))
      `(progn
         (import->defpackage ,package-name ,@body)
         ;; The helper macro must be expanded after package-name has
         ;; been defined.
         (import-as-package-aux ,package-name ,@body)))))

(defmacro import->defpackage (package-name
                              &body (&rest body
                                     &key
                                       ((:binding bindings))
                                       &allow-other-keys))
  (declare (ignore body))
  `(defpackage ,package-name
     (:use)
     (:export ,@(nub (loop for (nil alias) in bindings
                           collect (make-keyword
                                    (etypecase-of import-alias alias
                                      (var-alias alias)
                                      (function-alias (second alias))
                                      (macro-alias (second alias)))))))))

(defmacro import-as-package-aux (package-name &body
                                                (&rest body
                                                 &key ((:binding bindings))
                                                      &allow-other-keys))
  (let ((p (assure package (find-package package-name))))
    (labels ((intern* (sym)
               (intern (string sym) p))
             (intern-spec (spec)
               (loop for (key alias) in spec
                     collect `(,key :as ,(etypecase-of import-alias alias
                                           (var-alias (intern* alias))
                                           (function-alias
                                            (let ((alias (second alias)))
                                              `(function ,(intern* alias))))
                                           (macro-alias
                                            (let ((alias (second alias)))
                                              `(macro-function ,(intern* alias)))))))))
      (let ((module-binding (symbolicate '%module-for-package- (package-name p))))
        `(import ,module-binding
           :binding ,(intern-spec bindings)
           ,@body)))))

(defun subpackage-full-name (child-package-name)
  (let* ((parent-package *package*)
         (parent-package-name (package-name parent-package))
         (child-package-name (string child-package-name))
         (full-package-name
           (fmt "~a.~a" parent-package-name child-package-name)))
    (make-keyword full-package-name)))

(defmacro import-as-subpackage (child-package-name
                                &body body
                                &key
                                  &allow-other-keys)
  `(import-as-package ,(subpackage-full-name child-package-name)
     ,@body))
