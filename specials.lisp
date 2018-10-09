(defpackage #:vernacular/specials
  (:documentation "Special variables used throughout.")
  (:use #:cl #:overlord/types)
  (:import-from #:serapeum #:defvar-unbound)
  (:export
   #:*input*
   #:*output*
   #:*module*
   #:*program*
   #:*program-preamble*
   #:*language*
   #:*source*))
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
