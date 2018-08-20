#lang core-lisp

(defdynamic *count*
  (or (ignore-errors (dynamic *count*))
      -1))

(defun inc-count ()
  (setf (dynamic *count*)
        (+ (dynamic *count*) 1)))

(:export #'inc-count)
