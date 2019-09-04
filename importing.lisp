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
    :error*
    :absolute-pathname)
  (:import-from :overlord/freeze
    :*before-hard-freeze-hook*)
  (:import-from :vernacular/symbols
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
    (match spec
      ((eql :all)
       (loop for export in (get-static-exports)
             for sym = (intern (string export))
             collect `(,export :as ,sym)))
      ((eql :all-as-functions)
       (loop for export in (get-static-exports)
             for sym = (intern (string export))
             collect `(#',export :as #',sym)))
      ((list* :import-set import-sets)
       (mappend (op (expand-import-set _ #'get-static-exports))
                import-sets))
      ((type list)
       spec))))

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
                      (error* "Failed to guess source, and no language is specified.")))
                 source)))
      ;; We have the language, but not the source.
      ((and lang (no source))
       (values (resolve-lang lang)
               (resolve-source
                (or (guess-source lang module)
                    (error* "Failed to guess source, and no source is specified.")
                    (required-argument :from)))))
      ;; We have neither the language nor the source.
      ((nor lang source)
       (error* "We need a source file or a language.")))))

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
     bindings)))

(defun check-exports (source bindings exports)
  "Make sure the bindings are a subset of the exports."
  (let ((bindings (nub (mapcar #'ortho-keyword bindings))))
    (unless (subsetp bindings exports :test #'string=)
      (error 'binding-export-mismatch
             :source source
             :bindings bindings
             :exports exports))))

(defun check-static-bindings-1 (lang source bindings)
  (check-type lang keyword)
  (check-type source absolute-pathname)
  ;; (check-type bindings (satisfies setp))
  (unless (setp bindings :test #'equal)
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
  (let ((task-name
          (ematch module
            ((type symbol) module)
            ((list 'function name)
             name))))
    `(define-target-task ,task-name
       (setf ,module (require-as ',as ,from))
       (update-value-bindings ,module ,@values))))

(defmacro update-value-bindings (module &body values)
  `(progn
     ,@(collecting
         (dolist (clause values)
           (receive (ortho pseudo ref) (ortho+pseudo+ref clause module)
             (declare (ignore ortho))
             (collect
                 (ematch pseudo
                   ((type symbol) `(setf ,pseudo ,ref))
                   ((list 'function pseudo)
                    `(setf (symbol-function ',pseudo) ,ref))
                   ;; Do nothing. Macros cannot be imported as values.
                   ((list 'macro-function _)
                    nil))))))))

(defmacro import-bindings (module &body values)
  `(progn
     ,@(mapcar (op `(import-binding ,module ,_)) values)))

(defmacro import-binding (module clause)
  (receive (ortho pseudo ref) (ortho+pseudo+ref clause module)
    (declare (ignore ortho))
    (ematch pseudo
      ((type symbol)
       `(vernacular/shadows:def ,pseudo ,ref))
      ((list 'function pseudo)
       `(vernacular/shadows:defalias ,pseudo
          (assure function (function-wrapper ,ref))))
      ((list 'macro-function pseudo)
       (with-gensyms (whole body env)
         `(vernacular/shadows:defmacro ,pseudo (&whole ,whole &body ,body &environment ,env)
            (declare (ignore ,body))
            (funcall ,ref ,whole ,env)))))))

(defun apply-prefix (clauses prefix)
  (if (null prefix) clauses
      (flet ((prefix (suffix) (symbolicate prefix (assure symbol suffix))))
        (loop for clause in clauses
              collect (ematch clause
                        ((type symbol) (list clause :as (prefix clause)))
                        ((ns ns sym) (list clause :as `(,ns ,(prefix sym))))
                        ((list orig :as (and alias (type symbol)))
                         `(,orig :as ,(prefix alias)))
                        ((list orig :as (ns ns alias))
                         `(,orig :as (,ns ,(prefix alias)))))))))

(defun ortho+pseudo+ref (clause module)
  "Parse CLAUSE into three terms:
1. A (possibly qualified) orthonym.
2. A (possibly qualified) pseudonym.
3. A form to look up the orthonym of CLAUSE in MODULE."
  (let* ((key (ortho-keyword clause))
         (ns (ortho-namespace clause))
         (ref `(module-ref/inline-cache ,module ',key ',ns)))
    (values (ortho clause)
            (pseudo clause)
            ref)))

(defun ortho-keyword (clause)
  (values
   (ematch (ortho clause)
     ((and sym (type symbol))
      (if (eql (symbol-package sym)
               (find-package :vernacular/symbols))
          sym
          (make-keyword sym)))
     ((function-spec 'setf sym)
      (make-keyword sym))
     ((ns _ ortho)
      (make-keyword ortho)))))

(defun ortho-namespace (clause)
  (ematch (ortho clause)
    ((type symbol) nil)
    ((function-spec 'setf _)
     'setf)
    ((ns ns _) ns)))

(defun ortho (clause)
  (ematch clause
    ((type symbol) clause)
    ((ns _ _) clause)
    ((list ortho :as _)
     ortho)))

(defun pseudo (clause)
  (ematch clause
    ((type symbol) clause)
    ((ns _ _) clause)
    ((list _ :as pseudo)
     pseudo)))

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
                                       ((:binding bindings))
                                     &allow-other-keys))
  (declare (ignore body))
  `(defpackage ,package-name
     (:use)
     (:export ,@(nub (mapcar #'ortho-keyword bindings)))))

(defmacro import-as-package-aux (package-name &body
                                                (&rest body
                                                 &key ((:binding bindings))
                                                 &allow-other-keys))
  (let ((p (assure package (find-package package-name))))
    (labels ((intern* (sym)
               (intern (string sym) p))
             (intern-bindings (bindings)
               (loop for binding in bindings
                     for pseudo = (pseudo binding)
                     collect `(,(ortho binding)
                               :as ,(ematch pseudo
                                      ((type symbol) (intern* pseudo))
                                      ((ns ns symbol)
                                       `(,ns ,(intern* symbol))))))))
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
