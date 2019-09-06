#lang vernacular/simple-module

(:export #'make #'rows #'cols #'ref #'each (#'put! :as #'set!))

(defun make (n m)
  (let ((grid (make-array n)))
    (do ((i 0 (+ i 1)))
        ((= i n) grid)
      (let ((v (make-array m :initial-element nil)))
        (setf (aref grid i) v)))))

(defun rows (grid)
  (length grid))

(defun cols (grid)
  (length (aref grid 0)))

(defun ref (grid n m)
  (and (< -1 n (rows grid))
       (< -1 m (cols grid))
       (aref (aref grid n) m)))

(defun put! (grid n m v)
  (setf (aref (aref grid n) m) v))

(defun each (grid proc)
  (do ((j 0 (+ j 1)))
      ((= j (rows grid)))
    (do ((k 0 (+ k 1)))
        ((= k (cols grid)))
      (funcall proc j k (ref grid j k)))))
