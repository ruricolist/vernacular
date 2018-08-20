#lang cl

(vernacular/module:default-export-module
 (lambda (n)
   (labels ((fact (n)
              (if (<= n 1)
                  1
                  (* n (fact (- n 1))))))
     (fact n))))
