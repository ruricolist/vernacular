#lang core-lisp

;;; For use with imports.lsp.

(defglobal x :var)

(defun y ()
  :fn)

(defmacro z ()
  :macro)

(:export x #'y (macro-function z))
