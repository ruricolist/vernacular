#lang core-lisp

(defglobal b nil)

(let ((a 0))
  (macrolet ((mac (name)
               `(list ,(alias a) ,name)))
    (let ((a 1))
      ;; Should be '(0 1), not '(1 1).
      (setq b (mac a)))))

(:export-default b)
