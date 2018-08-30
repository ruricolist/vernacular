(defpackage :vernacular/lang
  (:use :cl :alexandria :serapeum
    :local-time
    :uiop/filesystem
    :uiop/pathname
    :overlord/target-protocol
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
  (:import-from :overlord/specials
    :*input*
    :*output*)
  (:import-from :vernacular/specials
    :*language*
    :*source*)
  (:export
   :lang :lang-name :hash-lang-name
   :load-module
   :expand-module
   :package-expander :package-reader :module-progn-in
   :with-meta-language
   :load-same-name-system
   :define-loader-language
   :*language* :*source*
   :read-lang-name
   :require-as :require-default
   :dynamic-require-as :dynamic-require-default :require-once
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
   :guess-lang+pos
   :resolve-lang
   :module-spec))
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

(defconstructor module-spec
  (lang lang-name)
  (path absolute-pathname))

(fset:define-cross-type-compare-methods module-spec)

(defmethod fset:compare ((spec1 module-spec) (spec2 module-spec))
  (nest
   (let-match1 (module-spec lang1 path1) spec1)
   (let-match1 (module-spec lang2 path2) spec2)
   (if (and (eql lang1 lang2)
            (equal path1 path2))
       :equal
       (let ((list1 (list lang1 path1))
             (list2 (list lang2 path2)))
         (declare (dynamic-extent list1 list2))
         (fset:compare list1 list2)))))

(defmethod target-exists? ((target module-spec))
  (~> target
      module-spec-cell
      module-cell.module))

(defmethod target-timestamp ((target module-spec))
  (let* ((cell (module-spec-cell target)))
    (with-slots (module timestamp) cell
      (if (null module) never timestamp))))

(defmethod (setf target-timestamp) (timestamp (target module-spec))
  (let ((cell (module-spec-cell target)))
    (setf (module-cell.timestamp cell) timestamp)))

(defmethod resolve-target ((target module-spec) &optional base)
  (let-match1 (module-spec lang source) target
    (if (absolute-pathname-p source) target
        (module-spec lang
                     (assure tame-pathname
                       (merge-pathnames* source
                                         (or base (base))))))))

(defmethod target= ((x module-spec) (y module-spec))
  (multiple-value-match (values x y)
    (((module-spec lang1 path1)
      (module-spec lang2 path2))
     (and (eql lang1 lang2)
          (pathname-equal path1 path2)))))

(defmethod hash-target ((target module-spec))
  (let-match1 (module-spec lang path) target
    (dx-sxhash (list 'module-spec lang path))))

(defmethod target-build-script ((target module-spec))
  (let ((cell (module-spec-cell target)))
    (with-slots (lang source) cell
      (task target
            (lambda ()
              ;; Prevent the old module object from persisting if
              ;; there is a problem building the new one.
              (unload-module-cell cell)
              (let ((*base* (pathname-directory-pathname source)))
                ;; Depend on the source file.
                (depends-on source)
                ;; Let the language tell you what to depend on.
                (lang-deps lang source))

              (let ((*language* lang))
                (load-module-into-cell cell)))
            trivial-prereq))))

(defmethod target-node-label ((target module-spec))
  (let-match1 (module-spec lang path) target
    (let ((path (native-namestring path)))
      (fmt "~a (#lang ~a)"
           path (string-downcase lang)))))

(defun hard-freeze-modules ()
  ;; The table of module cells needs special handling.
  (clear-module-cells)
  (clrhash (symbol-value '*claimed-module-names*)))

(add-hook '*before-hard-freeze-hook* 'hard-freeze-modules)


;;; Module cells.

;;; What's a module cell? What we want is a namespace for modules,
;;; indexed by language and source file. The obvious thing would be to
;;; declare a table (with `defvar') store modules there. Basically,
;;; that is what we do. But instead of simply storing the module in
;;; the table, we store an indirection -- a "module cell", a mutable
;;; cell which contains the actual module. Then, using compiler macros
;;; and `load-time-value', we can inject direct references to module
;;; cells into the compiled code. The result is that, for inline
;;; references to modules, no run-time lookup is necessary. This is
;;; key to keeping modules fast while also allowing for modules to be
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
   (lang
    :initarg :lang
    :type lang-name
    :reader module-cell.lang)
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

Each lang+source combination gets its own module cell with its own
unique identity.

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

;;; Compiler macro needs to appear as soon as possible to satisfy
;;; SBCL.
(define-compiler-macro module-cell (&whole call lang path)
  (cond ((packagep lang)
         `(module-cell ,(lang-name lang) ,path))
        ((or (quoted-symbol? lang) (keywordp lang))
         (typecase-of (or string cl:pathname) path
           (string `(module-cell ,lang ,(ensure-pathname path :want-pathname t)))
           (pathname
            (let ((path (resolve-target path (base)))) ;Resolve now, while `*base*' is bound.
              `(load-time-value
                (locally
                    ;; Prevent recursive expansion.
                    (declare (notinline module-cell))
                  ;; We can't use %ensure-module-cell directly,
                  ;; because it doesn't apply the defaults for the
                  ;; language.
                  (module-cell ,lang ,path)))))
           (otherwise call)))
        ((constantp lang)
         (let ((val (eval lang)))
           (if (eql val lang) call
               `(module-cell ,val ,path))))
        (t call)))

(defun module-spec-cell (spec)
  (let-match1 (module-spec lang path) spec
    (module-cell lang path)))

(defun module-cell-spec (cell)
  (with-slots (lang source) cell
    (module-spec lang source)))

(defun module-cell-meta (cell key)
  (synchronized (cell)
    (getf (module-cell.meta cell) key)))

(defun (setf module-cell-meta) (value cell key)
  (synchronized (cell)
    (setf (getf (module-cell.meta cell) key)
          value)))

(defplace module-meta (lang path key)
  (module-cell-meta (module-cell lang path) key))

(define-compiler-macro module-meta (lang path key)
  "Expand the call to module-cell at compile time so it can be
resolved at load time."
  `(module-cell-meta (module-cell ,lang ,path) ,key))

(define-compiler-macro (setf module-meta) (value lang path key)
  `(setf (module-cell-meta (module-cell ,lang ,path) ,key) ,value))

(defmethods module-cell (self lock source lang module)
  (:method initialize-instance :after (self &key)
    ;; Give the lock a name.
    (setf lock
          (bt:make-recursive-lock (fmt "Lock for module ~a" self))))

  (:method print-object (self stream)
    (print-unreadable-object (self stream :type t)
      (format stream "~a (~a) (~:[not loaded~;loaded~])"
              source
              lang
              module))))

(defun load-module-into-cell (cell)
  (lret ((module
          (validate-module
           (load-module (module-cell.lang cell)
                        (module-cell.source cell)))))
    (unload-module-cell cell)
    (setf
     (module-cell.module cell) module
     (module-cell.timestamp cell) (now))))

(defun unload-module-cell (cell)
  (with-slots (timestamp module) cell
    (clear-inline-caches (nix module))
    (setf timestamp never)))

(defun unload-module (lang source)
  (declare (notinline module-cell))
  (unload-module-cell (module-cell lang source)))

(defun %ensure-module-cell (lang path)
  "Get the module cell for LANG and PATH, creating and interning one
if it does not exist."
  (check-type path absolute-pathname)
  (setf path
        (assure pathname
          (or (truename* path)
              (or (progn
                    (build path)
                    (truename* path))
                  (error "Cannot resolve pathname ~a" path)))))
  (mvlet* ((key (cons lang path))
           (cell cell?
            (gethash key *module-cells*)))
    (if cell? (assure module-cell cell)
        (let ((cell (make 'module-cell :lang lang :source path)))
          (setf (gethash key *module-cells*) cell)))))

(defun module-cell (lang path)
  (let ((path
          (assure absolute-pathname
            (merge-input-defaults lang (ensure-pathname path :want-pathname t))))
        (lang (lang-name lang)))
    (%ensure-module-cell lang path)))

(defun find-module (lang source)
  (module-cell.module (module-cell lang source)))

(define-compiler-macro find-module (lang source)
  `(module-cell.module (module-cell ,lang ,source)))


;;; Languages

;;; Note that support for languages follows support for file patterns.
;;; A pattern is an abstract relationship between two files; a
;;; language is an abstract relationship between a file and Lisp
;;; binding.

(defun %require-as (lang source *base* &rest args)
  (ensure-pathnamef source)
  (apply #'dynamic-require-as
         lang
         (merge-pathnames* source *base*)
         args))

(defun dynamic-require-default (lang source &key force)
  (let ((module (dynamic-require-as lang source :force force)))
    (module-ref module :default)))

(defun lang+source (lang source)
  (check-type source (and absolute-pathname file-pathname))
  (let* ((lang (or lang (guess-lang+pos source)))
         (lang (lang-name lang)))
    (values lang source)))

(defun dynamic-require-as (lang source &key force)
  (receive (lang source) (lang+source lang source)
    (when force
      (dynamic-unrequire-as lang source))
    (assure (not module-cell)
      (let ((spec (module-spec lang source)))
        (depends-on spec)
        (module-cell.module
         (module-spec-cell spec))))))

(defun require-once (lang source)
  (receive (lang source) (lang+source lang source)
    (let ((spec (module-spec lang source)))
      (or (module-cell.module
           (module-spec-cell spec))
          (dynamic-require-as lang source)))))

(defun %unrequire-as (lang source *base*)
  (dynamic-unrequire-as lang
                        (merge-pathnames* source *base*)))

(defun dynamic-unrequire-as (lang source)
  (check-type source (and absolute-pathname file-pathname))
  (unload-module lang source)
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
  (let ((lang
          (resolve-lang lang))
        (source
          (assure absolute-pathname
            (ensure-pathname source))))
    (dynamic-require-as lang source)
    (values)))

(defmacro unrequire-as (lang source)
  "Wrap `%unrequire-as', resolving the base at macro-expansion time."
  `(%unrequire-as ,lang ,source ,(base)))

(defun escape-lang-name (lang-name)
  (check-type lang-name lang-name)
  (url-encode (string lang-name) :encoding :utf-8))

(defun lang-fasl-dir (lang current-dir)
  (let ((lang-string (escape-lang-name lang)))
    (shadow-tree-translate
     (make-shadow-tree :prefix (list "fasls" lang-string))
     (pathname-directory-pathname current-dir))))

(defun faslize (lang pathname)
  (etypecase-of lang lang
    (package
     (~> lang
         package-name-keyword
         (faslize pathname)))
    (lang-name
     (path-join (lang-fasl-dir lang pathname)
                (make-pathname :name (pathname-name pathname)
                               :type fasl-ext)))))

(defun fasl? (pathname)
  (let ((type (pathname-type pathname)))
    (and (stringp type)
         (string= type fasl-ext))))

(defun load-module (lang source)
  (ensure-pathnamef source)
  (let ((*base* (pathname-directory-pathname source)))
    (load-fasl-lang lang source)))

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

;;; This is a generic function so individual langs can define their
;;; own dependencies in :after methods.

;;; TODO Should this use the progn method combination?
(defgeneric lang-deps (lang source)
  (:method ((lang t) (source t))
    nil)

  (:method ((lang symbol) (source t))
    (lang-deps (resolve-lang-package lang) source)))

(defmethod pattern.input-defaults ((lang symbol))
  (let ((p (resolve-package lang)))
    (if p (pattern.input-defaults p) *nil-pathname*)))

(defmethod pattern.input-defaults ((p package))
  (extension
   (let ((sym (find-symbol #.(string 'extension) p)))
     (or (and sym (symbol-value sym))
         *nil-pathname*))))

(defmacro define-loader-language (package-name (source) &body (reader &key extension))
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
         :extension ,extension))))

(defmacro define-loader-language-1 (package-name (source) &body (reader &key extension))
  "The part that gets expanded once PACKAGE-NAME exists."
  (let ((p (find-package package-name)))
    (unless (packagep p)
      (error "This macro cannot expand until package ~a exists."
             package-name))
    (let ((syms (mapcar (op (find-symbol (string _) p))
                        (loader-language-exports)))
          (keyword (make-keyword package-name)))
      (destructuring-bind (load read ext script) syms
        `(progn
           (declaim (notinline ,load ,read))
           (eval-always
             (define-script ,script ,reader)
             (defparameter ,ext (extension ,extension))
             (defun ,load (,source)
               (default-export-module ,reader))
             (defun ,read (,source _stream)
               (declare (ignore _stream))
               (list ',load ,source))
             (defmethod lang-deps :after ((self (eql ,keyword)) source)
               (declare (ignore source))
               (depends-on ',script))))))))

(defun loader-language-table (val)
  (lambda (module key)
    (if (eql key :default) val
        (error 'no-such-export
               :key key
               :module module))))

(defun load-fasl-lang (lang source)
  (let ((object-file (faslize lang source)))
    (restart-case
        (load-as-module object-file)
      (recompile-object-file ()
        :report "Recompile the object file."
        (delete-file-if-exists object-file)
        (build (module-spec lang source))
        (load-fasl-lang lang source)))))

(defmethod lang-deps ((lang package) (source cl:pathname))
  (let* ((pat (fasl-lang-pattern lang source))
         (ref (pattern-ref pat source)))
    (depends-on ref)))

(defun lang-name (lang)
  (assure keyword
    (etypecase-of (or keyword lang-name package) lang
      (keyword lang)
      (lang-name (make-keyword lang))
      (package (make-keyword (package-name lang))))))

;;; This can't use `defpattern', for bootstrapping reasons.

(defclass fasl-lang-pattern (pattern)
  ((lang :initarg :lang
         :initform (required-argument :lang))
   (source :initarg :source))
  (:documentation "Pattern for building a fasl from a file. Note that
instances of this pattern must be parameterized with a language."))

(defmethods fasl-lang-pattern (self lang source)
  (:method pattern.output-defaults (self)
    (faslize lang source))

  (:method pattern-name (self)
    `(fasl-lang-pattern
      ,(maybe-delay-symbol (lang-name lang))
      ,source))

  (:method pattern-build (self)
    (let* ((*source* *input*)
           (lang (lang-name lang))
           (*language* lang)
           ;; Must be bound here for macros that intern
           ;; symbols.
           (*package* (user-package (resolve-package lang)))
           (*base* (pathname-directory-pathname *source*)))
      (depends-on *source*)
      (compile-to-file
       (wrap-current-module
        (expand-module lang *input*)
        lang *input*)
       (ensure-directories-exist *output*)
       :top-level (package-compile-top-level? lang)
       :source *source*))))

(defun fasl-lang-pattern (lang source)
  (let ((lang (force-symbol lang)))
    (make 'fasl-lang-pattern :lang lang :source source)))

(defun fasl-lang-pattern-ref (lang source)
  (pattern-ref (fasl-lang-pattern lang source) source))

(defmacro with-input-from-source ((stream source) &body body)
  "Read from SOURCE, skipping any #lang declaration."
  `(with-input-from-file (,stream ,source :element-type 'character)
     (skip-hash-lang ,stream)
     ,@body))

(def reader-string (string 'read-module))

(def module-string (string 'module-progn))

(def compile-top-level-string (string '*compile-top-level*))

(def loader-language-exports
  (list (string 'load)
        reader-string
        (string 'extension)
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
  (let ((eof "eof"))
    `(progn
       ,@(loop for form = (read stream nil eof)
               until (eq form eof)
               collect form))))

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
            (receive (sym status) (find-symbol reader-string p)
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
        (receive (sym status) (find-symbol module-string p)
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

(defun ensure-lang-exists (lang &optional (cont #'ensure-lang-exists))
  (check-type lang package-designator)
  (check-type cont function)
  (if (packagep lang) lang
      (let ((pkg (resolve-package lang)))
        (or (and pkg (package-name-keyword pkg))
            (restart-case
                (error 'no-such-lang :lang lang)
              (load-same-name-system ()
                :test maybe-find-asdf-system
                :report (lambda (s)
                          (format s "Load the system named ~a and try again" lang))
                (load-asdf-system lang)
                (funcall cont lang)))))))

(defun lookup-hash-lang (name)
  (assure (or null lang-name)
    (let* ((pkg-name (assure (satisfies valid-lang-name?)
                       ;; Set the case as if the string were being
                       ;; read, without using `read`.
                       (coerce-case name))))
      (ensure-lang-exists pkg-name #'lookup-hash-lang))))

(defun guess-lang+pos (file)
  "If FILE has a #lang line, return the lang and the position at which
the #lang declaration ends."
  (receive (lang pos)
      (file-hash-lang file)
    (if (stringp lang)
        (values (lookup-hash-lang lang) pos)
        (values nil 0))))

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
    (resolve-package (resolve-lang lang))))

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

(define-script-keyword-macro :module (lang source)
  ;; Is (base) right?
  `(module-spec ',lang
                (resolve-target (ensure-pathname ,source)
                                ,(base))))
