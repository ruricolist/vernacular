#lang vernacular/simple-module

(:export #'deref #'(setf deref))

(def storage 1)

(defun deref ()
  storage)

(defun (setf deref) (value)
  (setf storage value))
