(defpackage #:vernacular/specials
  (:use #:cl #:overlord/types)
  (:import-from #:serapeum #:defvar-unbound)
  (:export
   #:*input*
   #:*output*
   #:*module*
   #:*program*
   #:*program-preamble*
   #:*language*
   #:*source*
   #:*default-language*))
(in-package #:vernacular/specials)

(defvar-unbound *source* "Source file being compiled.")

(declaim (type absolute-pathname *source*))

(defvar-unbound *module* "The module being returned.")
(defvar-unbound *program* "The program to be compiled.")

(defvar-unbound *program-preamble*
  "A preamble to the program to be compiled.

This would be something like a package declaration, that the reader
has to see before the other forms.")

(defvar-unbound *language* "The name (symbol) of the current language.")
(declaim (type symbol *language*))

(defvar *default-language* nil
  "The name (symbol) of the language to use if the module file does not specify a language.")
(declaim (type symbol *default-language*))
