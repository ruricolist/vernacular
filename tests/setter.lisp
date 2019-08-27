#lang vernacular/package-module

(:export #'deref #'(setf deref-x))

(def storage 1)

(defun deref ()
  storage)

(defun (setf deref) (value)
  (setf storage value))
