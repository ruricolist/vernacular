#lang core-lisp

(defglobal syms '(x y z))

(defglobal x :right)

(defun y () :right)

(defmacro z () :right)

(defglobal xyz
  (lambda (form env) form env
     `(list ,(alias x)
            (,(function-alias y))
            (,(function-alias z)))))

(:export syms xyz)
