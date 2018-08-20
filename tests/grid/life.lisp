#lang vernacular/simple-module

(:import grid :from "grid.lisp"
  :binding :all-as-functions)
(:export #'life)

(defun life-count (grid i j)
  (flet ((cnt (i j)
           (if (ref grid i j) 1 0)))
    (+ (cnt (- i 1) (- j 1))
       (cnt (- i 1) j)
       (cnt (- i 1) (+ j 1))
       (cnt i (- j 1))
       (cnt i (+ j 1))
       (cnt (+ i 1) (- j 1))
       (cnt (+ i 1) j)
       (cnt (+ i 1) (+ j 1)))))

(defun life-alive? (grid i j)
  (case (life-count grid i j)
    (3 t)
    (2 (ref grid i j))
    (t nil)))

(defun life-print (grid &optional (separator #\Page))
  (write-string (string separator))
  (each grid
        (lambda (i j v) (declare (ignore i))
          (write-string (if v "*" " "))
          (when (= j (- (cols grid) 1))
            (terpri)))))

(defun life (grid iterations &optional (separator #\Page))
  (do ((i 0 (+ i 1))
       (grid0 grid grid1)
       (grid1 (make (rows grid) (cols grid))
              grid0))
      ((= i iterations))
    (each grid0
          (lambda (j k v) (declare (ignore v))
            (let ((a (life-alive? grid0 j k)))
              (set! grid1 j k a))))
    (life-print grid1 separator)))
