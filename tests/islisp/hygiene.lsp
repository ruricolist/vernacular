#lang core-lisp

(defmacro defcase (name args &body body) args
  `(defglobal ,name
     (or (ignore-errors
          ,@body)
         :error)))

(defcase case-1 ()
  (let ((x 3))
    (macrolet ((let-inc (u v)
                 `(let ((,u 2))
                    ,v)))
      (macrolet ((m (y)
                   `(let-inc x (* x ,y))))
        (m x)))))

(defcase case-2 ()
  (let ((x 3))
    (macrolet ((let-inc (u v)
                 `(let ((,u 2))
                    ,v)))
      (macrolet ((m (y)
                   (let ((x (gensym)))
                     `(let-inc ,x (* ,x ,y)))))
        (m x)))))

(defcase case-3 ()
  (let ((x 3))
    (macrolet ((let-inc (u v) v
                 `(+ 1 ,u)))
      (macrolet ((m (y)
                   (let ((x (gensym)))
                     `(let-inc ,x (* ,x ,y)))))
        (m x)))))

(defcase case-4 ()
  (let ((x 3))
    (macrolet ((let-inc (u v) v
                 `(+ 1 ,u)))
      (macrolet ((m (y)
                   `(let-inc x (* x ,y))))
        (m x)))))

(defcase case-5 ()
  (let ((x 3))
    (macrolet ((let-inc (u v)
                 `(let ((,u (+ 1 ,u))) ,v)))
      (macrolet ((m (y)
                   `(let-inc x (* x ,y))))
        ;; Improper capture of original X.
        (m x)))))

(defcase case-6 ()
  (let ((x 3))
    (macrolet ((let-inc (u v)
                 `(let ((,u (+ 1 ,u))) ,v)))
      (macrolet ((m (y)
                   (let ((x (gensym)))
                     `(let-inc ,x (* ,x ,y)))))
        ;; Reference to an unbound variable.
        (m x)))))

(:export-default (list case-1 case-2 case-3 case-4 case-5 case-6))
