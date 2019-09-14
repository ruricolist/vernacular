(defpackage :vernacular/include
  (:use :cl :alexandria :serapeum)
  (:import-from :overlord
    :depends-on
    :base
    :resolve-file)
  (:import-from :vernacular/lang
    :expand-module)
  (:import-from :vernacular/specials
    :*language*)
  (:export
   :include
   :splice))
(in-package :vernacular/include)

(defmacro include (path)
  "Read the module file at PATH using the current language (package),
inlining the resulting forms as if they had been written in the
current file (without a `module-progn').

Adds a build dependency on PATH."
  (let* ((path (resolve-file path))
         (module-form (expand-module path :lang *language*)))
    (depends-on path)
    ;; Drop the module-progn.
    (cons 'progn (rest module-form))))

(defmacro splice (path)
  "Read the module file at PATH using the syntax of its own language,
and inline the resulting forms as if they had been written in the
current file.

Adds a build dependency on PATH."
  (let* ((path (resolve-file path))
         (module-form (expand-module path)))
    (depends-on path)
    (cons 'progn (rest module-form))))
