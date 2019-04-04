(defpackage :vernacular/lang
  (:documentation "The core of Vernacular: handles parsing, compiling,
  and loading modules.")
  (:use :cl :alexandria :serapeum
    :local-time
    :uiop/filesystem
    :uiop/pathname
    :overlord/types
    :overlord/util
    :overlord/cache
    :overlord/stamp
    :overlord/asdf
    :overlord/base
    :overlord/global-state
    :overlord/target

    :vernacular/types
    :vernacular/import-set
    :vernacular/hash-lang-syntax
    :vernacular/module)
  ;; How to compile a program to a fasl.
  (:import-from :vernacular/compile-to-file
    :compile-to-file :load-as-module :fasl-ext)
  (:import-from :trivia
    :match :ematch :let-match1 :multiple-value-ematch
    :multiple-value-match)
  (:import-from :overlord/freeze
    :frozen?
    :*before-hard-freeze-hook*)
  (:import-from :overlord/specials
    :*base*)
  (:import-from :vernacular/specials
    :*language*
    :*default-lang*
    :*source*)
  (:import-from :vernacular/parsers
    :slurp-stream)
  (:import-from :vernacular/file-local
    :file-emacs-mode)
  (:export
   :lang :lang-name :hash-lang-name
   :load-module
   :expand-module :expand-module*
   :package-expander :package-reader :module-progn-in
   :with-meta-language
   :load-same-name-system
   :define-loader-language
   :*language* :*source*
   :read-lang-name
   :require-as :require-default
   :dynamic-require-as :dynamic-require-default :require-once
   :dynamic-unrequire
   ;; Module protocol.
   :module-meta

   :reintern :reinterning
   :*file-local-variables*
   :find-module

   ;; Emacs integration.
   :require-for-emacs
   :expand-module-for-emacs

   :module-dynamic-exports
   :faslize
   :ensure-lang-exists
   :guess-source
   :source-lang
   :resolve-lang
   :registered-lang
   :guess-lang
   :module-spec
   :module
   :loaded-modules-alist))
(in-package :vernacular/lang)


;;; Types.

(deftype lang-name ()
  ;; Keywords can be language names.
  '(and symbol (not (member t nil))))

(deftype hash-lang-name ()
  '(and lang-name (satisfies hash-lang-name?)))

(defun hash-lang-name? (x)
  (and (typep x 'lang-name)
       (valid-lang-name? (string x))))

(deftype lang ()
  '(or package lang-name))


;;; Module cells.

;;; What's a module cell? What we want is a namespace for modules,
;;; indexed by source file. The obvious thing would be to declare a
;;; table (with `defvar') and store modules there. Basically, that is
;;; what we do. But instead of simply storing the module in the table,
;;; we store an indirection -- a "module cell", a mutable cell which
;;; contains the actual module. Then, using compiler macros and
;;; `load-time-value', we can inject direct references to module cells
;;; into the compiled code. The result is that, for inline references
;;; to modules, no run-time lookup is necessary. This is key to
;;; keeping modules fast while also allowing for modules to be
;;; redefined. And ultimately it is similar to the strategy that Lisp
;;; itself uses to allow functions to be redefined at runtime without
;;; having to recompile all their callers.

(define-global-state *module-cells* (dict)
  "The global table of all module cells.")

(defun list-module-cells ()
  (hash-table-values *module-cells*))

;;; TODO Should this be a structure? But using a class gets us slot
;;; type checking on both SBCL and Clozure. A future optimization, but
;;; we can leave it for now.
(defclass module-cell ()
  ((timestamp
    :type target-timestamp
    :initform never
    :accessor module-cell.timestamp)
   (source
    :initarg :source
    :type (and file-pathname tame-pathname)
    :accessor module-cell.source)
   (meta
    :initform nil
    :type plist
    :accessor module-cell.meta
    :documentation "Metadata about the module. This persists even when
the module is reloaded.")
   (module
    :initform nil
    :accessor module-cell.module)
   (lock
    :reader monitor))
  (:documentation "Storage for a module.

Each source file gets its own module cell with its own unique
identity.

The module itself may be reloaded, but the module cell is interned
forever."))

(defun clear-module-cells ()
  "Delete information not needed at runtime from module cells."
  ;; We don't actually clear the table because there may be cases
  ;; where expansion of compiler macros has been suppressed by
  ;; optimization settings and there is no reference to the module
  ;; cell to keep it from being garbage-collected.
  (maphash (lambda (k mc) (declare (ignore k))
             (with-slots (source timestamp) mc
               (setf source *nil-pathname*
                     timestamp never)))
           *module-cells*))

(add-hook '*before-hard-freeze-hook* 'clear-module-cells)

;;; Compiler macro needs to appear as soon as possible to satisfy
;;; SBCL.
(define-compiler-macro module-cell (&whole call path)
  (typecase-of (or string pathname) path
    (string
     `(module-cell ,(ensure-pathname path :want-pathname t)))
    (pathname
     (let ((path (resolve-source path))) ;Resolve now, while `*base*' is bound.
       `(load-time-value
         (locally
             ;; Prevent recursive expansion.
             (declare (notinline module-cell))
           (module-cell ,path)))))
    (otherwise call)))

(defun module-cell-meta (cell key)
  (synchronized (cell)
    (getf (module-cell.meta cell) key)))

(defun (setf module-cell-meta) (value cell key)
  (synchronized (cell)
    (setf (getf (module-cell.meta cell) key)
          value)))

(defplace module-meta (path key)
  (module-cell-meta (module-cell path) key))

(define-compiler-macro module-meta (path key)
  "Expand the call to module-cell at compile time so it can be
resolved at load time."
  `(module-cell-meta (module-cell ,path) ,key))

(define-compiler-macro (setf module-meta) (value path key)
  `(setf (module-cell-meta (module-cell ,path) ,key) ,value))

(defmethods module-cell (self lock source module)
  (:method initialize-instance :after (self &key)
    ;; Give the lock a name.
    (setf lock
          (bt:make-recursive-lock (fmt "Lock for module ~a" self))))

  (:method print-object (self stream)
    (print-unreadable-object (self stream :type t)
      (format stream "~a (~:[not loaded~;loaded~])"
              source
              module))))

(defun load-module-into-cell (cell)
  (synchronized (cell)
    (lret* ((module
             (~> cell
                 module-cell.source
                 load-module
                 validate-module)))
      (unload-module-cell cell)
      (setf
       (module-cell.module cell) module
       (module-cell.timestamp cell) (now)))))

(defun unload-module-cell (cell)
  (synchronized (cell)
    (with-slots (timestamp module) cell
      (clear-inline-caches (nix module))
      (setf timestamp never))))

(defun loaded-modules-alist ()
  "Return an alist of (path . module-object) for all loaded modules."
  (loop for path being the hash-keys of *module-cells*
          using (hash-value cell)
        unless (eql never (module-cell.timestamp cell))
          collect (cons path (module-cell.module cell))))

(defun ensure-module-loaded (source)
  (ensure-module-cell-loaded (module-cell source)))

(define-compiler-macro ensure-module-loaded (source)
  `(ensure-module-cell-loaded (module-cell ,source)))

(defun ensure-module-cell-loaded (cell)
  (unless (module-cell.module cell)
    (synchronized (cell)
      (unless (module-cell.module cell)
        (load-module-into-cell cell)))))

(defun unload-module (source)
  (declare (notinline module-cell))
  (unload-module-cell (module-cell source)))

(defun intern-module-cell (path)
  "Get the module cell for PATH, creating and interning one
if it does not exist."
  (check-type path absolute-pathname)
  (setf path
        (assure pathname
          (or (truename* path)
              (or (progn
                    (build path)
                    (truename* path))
                  (error "Cannot resolve pathname ~a" path)))))
  (mvlet* ((cell cell?
            (gethash path *module-cells*)))
    (if cell? (assure module-cell cell)
        (let ((cell (make 'module-cell :source path)))
          (setf (gethash path *module-cells*) cell)))))

(defun module-cell (path)
  (let* ((path (ensure-absolute path)))
    (intern-module-cell path)))

(defun find-module (source)
  (module-cell.module (module-cell source)))

(define-compiler-macro find-module (source)
  `(module-cell.module (module-cell ,source)))


;;; Languages

;;; Note that support for languages follows support for file patterns.
;;; A pattern is an abstract relationship between two files; a
;;; language is an abstract relationship between a file and Lisp
;;; binding.

(defun %require-as (lang source *base* &rest args)
  (apply #'dynamic-require-as
         lang
         source
         args))

(defun dynamic-require-default (lang source &key force)
  (let ((module (dynamic-require-as lang source :force force)))
    (module-ref module :default)))

(defun dynamic-require-as (lang source &key force)
  (let* ((*default-lang* (and lang (lang-name lang)))
         (source (resolve-source source)))
    (when force
      (dynamic-unrequire source))
    (depends-on (fasl-lang-pattern-ref source))
    (ensure-module-loaded source)
    (find-module source)))

(defun require-once (lang source)
  (let ((source (resolve-source source)))
    (or (find-module source)
        (dynamic-require-as lang source))))

(defun %unrequire (source *base*)
  (dynamic-unrequire (resolve-source source)))

(defun dynamic-unrequire (source)
  "Unload the module at SOURCE."
  (check-type source (and absolute-pathname file-pathname))
  (unload-module source)
  (values))

(defmacro require-as (&rest args)
  "Wrap `%require-as`, resolving the base at macro-expansion time.
A single arg is treated as the source, with the language being inferred.
Two args is treated as the language and the source."
  (receive (lang source)
      (ematch args
        ((list source)
         (values nil source))
        ((list lang source)
         (values lang source)))
    `(%require-as ,lang ,source ,(base))))

(defmacro require-default (&rest args)
  `(module-ref* (require-as ,@args) :default))

(defun require-for-emacs (lang source)
  "Like `dynamic-require-as', but with looser restrictions for easy
interoperation with Emacs."
  (dynamic-require-as lang source)
  (values))

(defmacro unrequire (source)
  "Wrap `%unrequire', resolving the base at macro-expansion time."
  `(%unrequire ,source ,(base)))

(defun escape-lang-name (lang-name)
  (check-type lang-name lang-name)
  (url-encode (string lang-name) :encoding :utf-8))

(defun vernacular-major-version ()
  (version-major-version
   (asdf-system-version "vernacular")))

(defun fasl-dir (current-dir)
  (let ((version (princ-to-string (vernacular-major-version))))
    (shadow-tree-translate
     (make-shadow-tree :prefix `("vernacular" ,version "fasls"))
     (pathname-directory-pathname current-dir))))

(defun faslize (pathname)
  (make-pathname :defaults (fasl-dir pathname)
                 :name (pathname-name pathname)
                 :type fasl-ext))

(defun fasl? (pathname)
  (equal (pathname-type pathname)
         fasl-ext))

(defun load-module (source)
  (ensure-pathnamef source)
  (let ((*base* (pathname-directory-pathname source)))
    (load-fasl-lang source)))

(defmethod module-static-exports (lang source)
  (check-type source absolute-pathname)
  (let ((lang (resolve-lang-package lang)))
    (if-let (sym (find-external-symbol (string 'static-exports) lang))
      ;; If the language exports a function to parse static exports,
      ;; use it.
      (values (funcall sym source) t)
      (values nil nil))))

(defun module-dynamic-exports (lang source)
  (module-exports* (dynamic-require-as lang source)))


;;; Languages.

(defun resolve-source (source)
  (resolve-file source))

;;; This is a generic function so individual langs can define their
;;; own dependencies in :after methods.

;;; TODO Should this use the progn method combination?
(defgeneric lang-deps (lang source)
  (:method ((lang t) (source t))
    nil)

  (:method ((lang symbol) (source t))
    (lang-deps (resolve-lang-package lang) source)))

(defmacro define-loader-language (package-name (source) &body (reader &rest keys &key &allow-other-keys))
  "Define PACKAGE-NAME as a package implementing a very simple language.

When loading SOURCE, with the language package bound as the current
package, the value returned by READER is used as the module.

E.g. a language that just loaded a text file as a string:

    (define-loader-language :text-file (source)
      (alexandria:read-file-into-string source))

Does some sanity checking on PACKAGE-NAME to make sure an existing
package is not overwritten."
  (let* ((pn (string package-name)))
    ;; Sanity check: are we overwriting an existing package?
    (when-let (package (find-package pn))
      (when (package-use-list package)
        (error* "Package already exists with a use list: ~a" package))
      (unless (set-equal (package-exports package)
                         (loader-language-exports)
                         :test #'string=)
        (error* "Package already exists with wrong exports: ~a" package)))
    `(progn
       (defpackage ,pn
         (:use)
         (:export ,@(loader-language-exports)))
       (define-loader-language-1 ,pn (,source)
         ,reader
         ,@keys))))

(defmacro define-loader-language-1 (package-name (source) &body (reader &rest keys &key &allow-other-keys))
  "Auxiliary macro for `define-loader-language'.

The part that can only be expanded once PACKAGE-NAME exists as a
package."
  (declare (ignore keys))
  (let ((p (find-package package-name)))
    (unless (packagep p)
      (error "This macro cannot expand until package ~a exists."
             package-name))
    (let ((syms (mapcar (op (find-symbol (string _) p))
                        (loader-language-exports)))
          (keyword (make-keyword package-name)))
      (destructuring-bind (load read script) syms
        `(progn
           (declaim (notinline ,load ,read))
           (eval-always
             (define-script ,script ,reader)
             (defun ,load (,source)
               (default-export-module ,reader))
             (defun ,read (,source _stream)
               (declare (ignore _stream))
               (list ',load ,source))
             (defmethod lang-deps :after ((self (eql ,keyword)) source)
               (declare (ignore source))
               (depends-on ',script))))))))

(defun load-fasl-lang (source)
  (let* ((object-file (faslize source)))
    (restart-case
        (load-as-module object-file)
      (recompile-object-file ()
        :report "Recompile the object file."
        (delete-file-if-exists object-file)
        (build (fasl-lang-pattern-ref source))
        (load-fasl-lang source)))))

(defun lang-name (lang)
  (assure keyword
    (etypecase-of (or keyword lang-name package) lang
      (keyword lang)
      (lang-name (make-keyword lang))
      (package (make-keyword (package-name lang))))))

(defclass fasl-lang-pattern (pattern)
  ())

(defmethods fasl-lang-pattern (self)
  (:method pattern-build (self sources outputs)
    (let* ((source (only-elt sources))
           (output (only-elt outputs))
           (source (resolve-source source))
           (lang (source-lang source))
           (*source* source)
           (*language* lang)
           ;; Must be bound here for macros that intern
           ;; symbols.
           (*package* (user-package (resolve-package lang)))
           (*base* (pathname-directory-pathname *source*)))
      (assert (file-exists-p source) ()
              "File ~a does not exist" source)
      ;; Depend on the source file.
      (depends-on source)
      ;; Depend on the computed language.
      (depends-on (language-oracle source))
      ;; Let the language tell you what to depend on.
      (lang-deps lang source)
      (compile-to-file
       (wrap-current-module
        (expand-module lang source)
        lang output)
       (ensure-directories-exist output)
       :top-level (package-compile-top-level? lang)
       :source *source*)
      ;; XXX There really should be an in-Lisp binding that is
      ;; rebuilt, instead of the module cell being side-effected.
      (unload-module source)))

  ;; TODO merge-input-defaults using the language?
  (:method merge-input-defaults (self (sources sequence))
    (map 'list #'resolve-source sources))

  (:method merge-output-defaults (self (sources sequence))
    (map 'list #'faslize sources)))

(declaim (notinline source-lang-for-oracle))
(defun source-lang-for-oracle (source)
  ;; It would be tempting to try to resolve the language name to a
  ;; package here, in case the language name is a nickname. It would
  ;; be nice if we could detect that a name points to a different
  ;; package. But we don't want to require that a language package be
  ;; loaded in order to load modules compiled using that language.
  (assure keyword
    (source-lang source)))

(defun language-oracle (source)
  (overlord:function-oracle 'source-lang-for-oracle source))

(defun module-spec (lang source)
  (let ((*default-lang* (or lang *default-lang*)))
    (fasl-lang-pattern-ref source)))

(defun fasl-lang-pattern-ref (source)
  (pattern-ref (make 'fasl-lang-pattern) source))

(defmacro with-input-from-source ((stream source) &body body)
  "Read from SOURCE, skipping any #lang declaration."
  `(with-input-from-file (,stream ,source :element-type 'character)
     (skip-hash-lang ,stream)
     ,@body))

(def package-reader-string (string 'read-module))

(def package-expander-string (string 'module-progn))

(def compile-top-level-string (string '*compile-top-level*))

(def loader-language-exports
  (list (string 'load)
        package-reader-string
        (string 'script)))

;;; Make it a function so it can be used before defined.
(defun loader-language-exports ()
  loader-language-exports)

(defmacro module-progn-in (package &body body &environment env)
  "Resolve a package's expander at macro-expansion time.
Also, ensure that PACKAGE is the current package when BODY is
macro-expanded.

If PACKAGE does not export an expander, `progn' is used instead."
  ;; Is expanding the macro this way useful?
  (let* ((package-expander (package-expander package :errorp nil))
         (module-progn (or package-expander 'progn))
         (form `(,module-progn ,@body)))
    (expand-in-package form package env)))

(defun suffix-package (package suffix)
  "Like `resolve-package' but, if a package exists with the same name,
but ending in SUFFIX, and inheriting from that package, return that
instead."
  (assert (string^= "-" suffix))
  (assure package
    (with-absolute-package-names ()
      (when-let (base-package (resolve-package package))
        (let* ((user-package-name
                 (concatenate 'string
                              (package-name base-package)
                              suffix))
               (user-package (find-package user-package-name)))
          (or (and user-package
                   (find base-package (package-use-list user-package))
                   user-package)
              base-package))))))

(defun user-package (package)
  "Like `resolve-package' but, if a package exists with the same name,
but ending in `-user', and inheriting from that package, return that
instead."
  (suffix-package package "-USER"))

;;; TODO Is this useful?
(defun expand-in-package (form package env)
  (let ((*package* (user-package (resolve-package package))))
    (macroexpand-1 form env)))

(defun cl-read-module (source stream)
  (declare (ignore source))
  `(progn ,@(slurp-stream stream)))

(defun package-compile-top-level? (package)
  (and-let* ((sym (find-symbol compile-top-level-string package))
             ((boundp sym)))
    (symbol-value sym)))

(defun package-reader (package &key (errorp t))
  "Resolve the reader exported by PACKAGE."
  (flet ((error* (&rest args)
           (if errorp
               (apply #'error* args)
               (return-from package-reader nil))))
    (assure (or symbol null)
      (let ((p (resolve-package package)))
        (if (eql p (find-package :cl))
            'cl-read-module
            (receive (sym status) (find-symbol package-reader-string p)
              (cond ((no sym)
                     ;; There is no symbol.
                     (error* "No reader defined in package ~a" p))
                    ((not (eql status :external))
                     ;; There is a symbol, but it's not external.
                     (error* "Package ~a does not export a reader" p))
                    ((not (fboundp sym))
                     ;; There is an external symbol, but it's not
                     ;; fbound.
                     (error* "No binding for reader in package ~a" p))
                    (t sym))))))))

(defun reintern (s &aux (p *package*))
  (let ((s (string s)))
    (or (find-symbol s p)
        (error "No symbol named ~a in ~s" s p))))

(defmacro reinterning ((&rest names) &body body)
  `(let ,(loop for name in names
               collect `(,name (reintern ',name)))
     ,@body))

(defun package-expander (package &key (errorp t))
  "Resolve the expander exported by PACKAGE."
  (flet ((error* (&rest args)
           (if errorp
               (apply #'error* args)
               (return-from package-expander nil))))
    (assure (or symbol null)
      (let ((p (resolve-package package)))
        (receive (sym status) (find-symbol package-expander-string p)
          (cond ((no sym)
                 (error* "No expander defined in package ~a" p))
                ((not (eql status :external))
                 (error* "Package ~a does not export an expander" p))
                ((not (fboundp sym))
                 (error* "Expander in package ~a is exported but unbound" p))
                ((not (macro-function sym))
                 (error* "Package ~a exports an expander that is not a macro" p))
                (t
                 (unless (eql (symbol-package sym) p)
                   (simple-style-warning "Package expander ~a in ~a is inherited from ~a."
                                         sym p (symbol-package sym)))
                 sym)))))))

(defparameter *file-local-variables*
  '(*package* *readtable*
    ;; These seem like a good idea to me.
    *read-base* *read-default-float-format*
    *file-local-variables*)
  "Variables that should be given fresh rebindings while reading in a
  module.

This should be a superset of the variables bound by CL during calls to
`cl:load'.")

(defun expand-module (package source
                      &key ((:in base))
                      &aux (file-locals *file-local-variables*))
  ;; Specifying the base (for interactive use).
  (when base
    (nlet lp (base)
      (etypecase-of (or directory-pathname string-designator package)
          base
        (directory-pathname
         (setf source (merge-pathnames* source base)))
        (string-designator
         (lp (find-package base)))
        (package
         (lp (package-base base))))))

  (let* ((package (resolve-package package))
         (*language* (lang-name package))
         (source (ensure-pathname source :want-pathname t))
         (*source* source))
    (with-input-from-source (in source)
      (progv file-locals (mapcar #'symbol-value file-locals)
        (let* ((reader (package-reader package))
               (module-form
                 (let ((*package* (user-package package)))
                   (funcall reader source in))))
          module-form)))))

(defun expand-module* (source)
  (expand-module (source-lang source) source))

(defun expand-module-for-emacs (lang source)
  (setf lang (resolve-lang lang))
  (values (expand-module lang source)))

(defmacro with-current-module ((lang source) &body body)
  `(macrolet ((current-module-lang () ',lang)
              (current-module-source () ',source)
              (current-module-cell ()
                `(module-cell ',',lang ,',source))
              (current-module ()
                `(find-module ',',lang ,',source))
              (current-module-meta (key)
                `(module-meta ',',lang ,',source ,key)))
     ,@body))

(defun wrap-current-module (form package source)
  (let ((lang (lang-name package)))
    `(with-current-module (,lang ,source)
       ,form)))


;;; #lang syntax.

(defcondition no-such-lang (overlord-error)
  ((lang :initarg :lang :type string-designator
         :reader no-such-lang.lang))
  (:report (lambda (c s)
             (with-slots (lang) c
               (format s "No such language as ~a" lang)))))

(defun load-same-name-system (c)
  (declare (ignore c))
  (invoke-restart 'load-same-name-system))

(defgeneric maybe-find-asdf-system (lang)
  (:method ((lang no-such-lang))
    (maybe-find-asdf-system (no-such-lang.lang lang)))
  (:method ((lang t))
    (and (not (frozen?))
         (let ((lang (string-downcase lang)))
           (find-asdf-system lang)))))

(defun call/load-same-name-system-restart (fn system)
  (nlet retry ()
    (restart-case
        (funcall fn)
      (load-same-name-system ()
        :test maybe-find-asdf-system
        :report (lambda (s)
                  (format s "Load the system named ~a and try again" system))
        (load-asdf-system system)
        (retry)))))

(defmacro with-load-same-name-system-restart ((system-name) &body body)
  (with-thunk (body)
    `(call/load-same-name-system-restart ,body ,system-name)))

(defun ensure-lang-exists (lang)
  (check-type lang package-designator)
  (if (packagep lang) lang
      (let ((pkg (resolve-package lang)))
        (or (and pkg (package-name-keyword pkg))
            (error 'no-such-lang :lang lang)))))

(defun lookup-hash-lang (name)
  (assure (or null lang-name)
    (let* ((pkg-name (assure (satisfies valid-lang-name?)
                       ;; Set the case as if the string were being
                       ;; read, without using `read`.
                       (coerce-case name))))
      (with-load-same-name-system-restart (pkg-name)
        (ensure-lang-exists pkg-name)))))

(defun guess-lang+pos (file)
  "If FILE has a #lang line (or, failing that, a -*- mode: -*- line),
return the lang and the position at which the #lang declaration ends."
  (receive (lang pos)
      (file-hash-lang file)
    (if (stringp lang)
        (values (make-keyword (coerce-case lang)) pos)
        (if-let (lang (file-emacs-mode-lang file))
          (values lang 0)
          (values nil 0)))))

(defun guess-lang (source)
  (values (guess-lang+pos source)))

(defcondition source-without-lang (vernacular-error)
  ((source :initarg :source :type pathname))
  (:report (lambda (self stream)
             (with-slots (source) self
               (format stream "Source file ~a does not specify a ~
         language. You will have to specify a language when ~
         importing instead." source)))))

(defun file-emacs-mode-lang (source)
  (when-let (name (file-emacs-mode source))
    (make-keyword (string-upcase name))))

(defun source-lang (source &optional (default *default-lang*))
  (let ((source (resolve-file source)))
    (lang-name
     (or (guess-lang source)
         default
         (error 'source-without-lang :source source)))))

(defun guess-source (lang alias)
  (~>> (etypecase-of import-alias alias
         (var-alias alias)
         ((or function-alias macro-alias)
          (second alias)))
       string-downcase
       (make-pathname :name)
       (merge-input-defaults lang)))

(defun resolve-lang (lang)
  (assure lang-name
    (etypecase-of (or lang-name string) lang
      (string (lookup-hash-lang lang))
      (lang-name lang))))

(defun resolve-lang-package (lang)
  (assure package
    (with-load-same-name-system-restart (lang)
      (nlet retry ()
        (let ((package (resolve-package (resolve-lang lang))))
          (if (packagep package) package
              (restart-case
                  (error 'no-such-lang :lang lang)
                (continue ()
                  :report "Try again"
                  (retry)))))))))

(defmacro with-meta-language ((path stream) &body body)
  (with-thunk (body path stream)
    `(call/meta-language ,body ,path ,stream)))

(defun call/meta-language (fn path stream)
  (let* ((next-lang (read-lang-name stream))
         (package (resolve-lang-package next-lang))
         (user-package (user-package package))
         (*package* user-package)
         (forms (funcall fn path stream)))
    `(module-progn-in ,(package-name-keyword package)
       ,@forms)))

(defun module (source)
  (~> source
      resolve-file
      fasl-lang-pattern-ref))
