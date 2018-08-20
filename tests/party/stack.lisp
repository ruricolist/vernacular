#lang vernacular/simple-module

(:export #'make #'push! #'pop! #'empty!)

(defun make ()
  (list '()))

(defun push! (s v)
  (setf (car s) (cons v (car s))))

(defun pop! (s)
  (let ((v (caar s)))
    (setf (car s) (cdar s))
    v))

(defun empty! (s)
  (setf (car s) '()))
