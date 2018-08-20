#lang vernacular/simple-module

(:export #'make #'push #'pop)

(defun make (w h)
  (cons w h))

(defun push (b amt)
  (cons (- (car b) amt) (+ (cdr b) amt)))

(defun pop (b)
  (format nil "Boom! ~a" (* (car b) (cdr b))))
