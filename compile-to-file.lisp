(defpackage #:vernacular/compile-to-file
  (:use #:cl #:alexandria #:serapeum
    #:vernacular/specials
    #:vernacular/file-package)
  (:import-from #:overlord/types #:absolute-pathname)
  (:import-from #:overlord/asdf #:asdf-system-relative-pathname)
  (:import-from #:trivial-macroexpand-all #:macroexpand-all)
  (:export #:compile-to-file #:load-as-module :fasl-ext :module))
(in-package #:vernacular/compile-to-file)

(defconst no-module "no-module")

(defun module? (x)
  (not (eq x no-module)))

(deftype module ()
  '(satisfies module?))

(defconst fasl-ext
  (pathname-type
   (compile-file-pathname
    "foo.lisp"))
  "The extension of a fasl in this Lisp.")

;;; http://kpreid.livejournal.com/14713.html
(def universal-file
  (asdf-system-relative-pathname :vernacular "universal.lisp"))

(defun compile-to-file (program output-file
                        &key source
                        &aux (namestring (namestring source)))
  "Compile PROGRAM to a fasl."
  (check-type source absolute-pathname)
  ;; TODO Would it be worth it to macroexpand the program in
  ;; advance with `macroexpand-all', to minimize time spent
  ;; inside the compiler lock?
  #+(or) (setf program (macroexpand-all program))
  (let* ((package *package*)
         (*program-preamble*
           ;; Ensure that a file package exists whenever compiling at
           ;; the top level.
           `(progn (define-file-package ,source)
                   (in-file-package ,source)))
         (*program*
           `(progn
              ,program
              (setq *module* (find-file-package ,source)))))
    (synchronized ()
      ;; TODO The following is cribbed from the sources of Slime and Sly.
      (with-compilation-unit (:allow-other-keys t
                              ;; SBCL
                              :source-namestring namestring)
        (let ((*package* package)
              (*readtable* (copy-readtable nil)))
          (compile-file universal-file
                        :allow-other-keys t
                        :output-file output-file
                        :external-format :utf-8
                        ;; CCL.
                        :compile-file-original-truename source
                        ;; ECL.
                        :source-truename source
                        ;; Clasp.
                        :source-debug-namestring namestring))))))

(defun load-as-module (file)
  "Load FILE and return whatever it assigns to `*module*'."
  (let ((*module* no-module))
    (load file :external-format :utf-8)
    (if (packagep *module*)
        *module*
        (error "No module in ~a" file))))
