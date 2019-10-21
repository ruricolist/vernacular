(defpackage :vernacular/importing
  (:documentation "Macros that handle importing from modules.")
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
  (:import-from :trivia
    :match :ematch)
  (:shadowing-import-from :trivia
    :defpattern)
  (:import-from :overlord/types
    :absolute-pathname)
  (:import-from :overlord/freeze
    :*before-hard-freeze-hook*)
  (:import-from :vernacular/well-known-exports
    :default)
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

(defcondition importing-error (vernacular-error)
  ())

(defcondition duplicated-bindings (importing-error)
  ((bindings :initarg :bindings))
  (:report (lambda (c s)
             (with-slots (bindings) c
               (format s "Duplicate bindings in ~a" bindings)))))

(defcondition binding-export-mismatch (importing-error)
  ((source :initarg :source)
   (bindings :initarg :bindings :type list)
   (exports :initarg :exports :type list))
  (:report (lambda (c s)
             (with-slots (bindings exports source) c
               (format s "Requested bindings do not match exports.~%Source: ~a~%Bindings: ~s~%Exports: ~s"
                       source bindings exports)))))

(defcondition not-enough-info (importing-error)
  ((lang :initarg :lang)
   (source :initarg :source))
  (:report (lambda (c s)
             (with-slots (lang source) c
               (format s "Not enough information: you must specify either a language or a source.~%Language:~a~%Source:~a"
                       lang source)))))

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
  (expand-import-set
   spec
   (lambda ()
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
           (module-dynamic-exports lang source))))))

(defmacro function-wrapper (fn)
  "Global definition for possible shadowing."
  fn)

(define-global-state *claimed-module-names* (make-hash-table :size 1024)
  "Table to track claimed modules, so we can warn if they are
  redefined.")

(defun claim-module-name (module source)
  "Warn if SOURCE is already bound to a different MODULE."
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
                  (or (guess-lang source)
                      (error 'not-enough-info :lang lang :source source)))
                 source)))
      ;; We have the language, but not the source.
      ((and lang (no source))
       (values (resolve-lang lang)
               (resolve-source
                (or (guess-source lang module)
                    (error 'not-enough-info :lang lang :source source)
                    (required-argument :from)))))
      ;; We have neither the language nor the source.
      ((nor lang source)
       (error 'not-enough-info :lang lang :source source)))))

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
      (apply-prefix prefix)))

(defmacro check-static-bindings-now (lang source bindings)
  "Wrapper around check-static-bindings to force evaluation at compile time.
Can't use eval-when because it has to work for local bindings."
  (check-static-bindings lang source bindings))

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
     bindings)))

(defun check-exports (source bindings exports)
  "Make sure the bindings are a subset of the exports."
  (unless (subsetp (mapcar #'private-name bindings)
                   (mapcar #'public-name exports)
                   :test #'string=)
    (error 'binding-export-mismatch
           :source source
           :bindings bindings
           :exports exports)))

(defun check-static-bindings-1 (lang source bindings)
  (check-type lang keyword)
  (check-type source absolute-pathname)
  (unless (setp bindings :test #'equal)
    (error 'duplicated-bindings :bindings bindings))
  (receive (static-exports exports-statically-known?)
      (module-static-exports lang source)
    (if exports-statically-known?
        (check-exports source bindings static-exports)
        (restart-case
            (let ((exports (module-dynamic-exports lang source)))
              (check-exports source bindings exports))
          (recompile-object-file ()
            :report "Recompile the object file."
            (let ((object-file (faslize source))
                  (target (compiled-module-target source)))
              (delete-file-if-exists object-file)
              (build target)
              (check-static-bindings lang source bindings)))))))

(defmacro declaim-module (source)
  `(propagate-side-effect
     (ensure-target-recorded
      (compiled-module-target ,source))))

(defmacro import-module (module &body (&key as from once))
  "When ONCE is non-nil, the module will only be rebuilt if it has not
yet been loaded."
  (check-type module symbol)
  (let ((req-form
          (if once
              `(require-once ',as ,from)
              `(require-as ',as ,from))))
    `(progn
       (vernacular/shadows:def ,module ,req-form)
       (declaim-module ,from)
       ',module)))

(defmacro import-default (var &body (&key as from))
  (let ((module-name (symbolicate '__module-for- var)))
    `(import ,module-name
       :as ,as
       :from ,from
       :binding ((default :as ,var)))))

(defmacro import-task (module &body (&key as from values))
  (let ((task-name (public-name module)))
    `(define-target-task ,task-name
       (setf ,module (require-as ',as ,from))
       (update-value-bindings ,module ,@values))))

(defmacro update-value-bindings (module &body values)
  `(progn
     ,@(collecting
         (loop for clause in values
               for name = (public-name clause)
               for ns = (public-ns clause)
               for ref = (private-ref clause module)
               collect (ecase ns
                         ((nil) `(setf ,name ,ref))
                         ((function)
                          `(setf (symbol-function ',name) ,ref))
                         ((setf)
                          `(setf (fdefinition '(setf ,name)) ,ref))
                         ;; Do nothing. Macros cannot be imported as values.
                         ((macro-function)
                          nil))))))

(defmacro import-bindings (module &body values)
  `(progn
     ,@(mapcar (op `(import-binding ,module ,_)) values)))

(defmacro import-function (name ref)
  `(vernacular/shadows:defalias ,name
     (assure function (function-wrapper ,ref))))

(defmacro import-binding (module clause)
  (let ((ns (public-ns clause))
        (name (public-name clause))
        (ref (private-ref clause module)))
    (ecase ns
      ((nil)
       `(vernacular/shadows:def ,name ,ref))
      (function
       `(import-function ,name ,ref))
      (setf
       `(import-function (setf ,name) ,ref))
      (macro-function
       (with-gensyms (whole body env)
         `(vernacular/shadows:defmacro ,name (&whole ,whole &body ,body &environment ,env)
            (declare (ignore ,body))
            (funcall ,ref ,whole ,env)))))))

(defun apply-prefix (clauses prefix)
  (if (null prefix) clauses
      (flet ((prefix (suffix) (symbolicate prefix (assure symbol suffix))))
        (loop for clause in clauses
              for private = (private-side clause)
              for ns = (public-ns clause)
              for name = (prefix (public-name clause))
              collect `(,private :as (,ns ,name))))))

(defun private-ref (clause module)
  (let* ((key (private-keyword clause))
         (ns (private-ns clause)))
    `(module-ref/inline-cache ,module ',key ',ns)))

(defun private+public+ref (clause module)
  "Parse CLAUSE into three terms:
1. A (possibly qualified) private name.
2. A (possibly qualified) public name.
3. A form to look up the private name of CLAUSE in MODULE."
  (values (private-side clause)
          (public-side clause)
          (private-ref clause module)))

(defun private-keyword (clause)
  (let ((sym (private-name clause)))
    (if (eql (symbol-package sym)
             (find-package :vernacular/well-known-exports))
        sym
        (make-keyword sym))))

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
                    :binding ((default :as ,bind)))
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
                                       nicknames
                                       documentation
                                       ((:binding bindings))
                                     &allow-other-keys))
  (declare (ignore body))
  `(defpackage ,package-name
     (:use)
     (:nicknames ,@nicknames)
     ,@(unsplice (and documentation `(:documentation ,documentation)))
     (:export ,@(nub (mapcar #'private-keyword bindings)))))

(defmacro import-as-package-aux (package-name &body
                                                (&rest body
                                                 &key ((:binding bindings))
                                                 &allow-other-keys))
  (let ((p (assure package (find-package package-name))))
    (labels ((intern* (sym)
               (intern (string sym) p))
             (intern-bindings (bindings)
               (loop for binding in bindings
                     collect `(,(private-side binding)
                               :as (,(public-ns binding)
                                    ,(intern* (public-name binding)))))))
      (let ((module-binding (symbolicate '%module-for-package- (package-name p))))
        `(import ,module-binding
           :binding ,(intern-bindings bindings)
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
