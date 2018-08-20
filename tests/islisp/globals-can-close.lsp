#lang core-lisp

(defglobal x (lambda () x))

(:export x)
