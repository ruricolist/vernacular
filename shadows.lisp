(uiop:define-package :vernacular/shadows
    (:import-from :alexandria)
  (:nicknames :vernacular/cl)
  (:shadow
   #:module-progn

   #:defun
   #:defmacro
   #:defconstant
   #:define-symbol-macro
   #:def #:define-values #:defconst     ;Serapeum.
   #:defsubst #:defalias
   #:define-constant                    ;Alexandria.


   #:let
   #:let*
   #:flet
   #:labels
   #:macrolet
   #:symbol-macrolet

   #:progn
   #:locally
   #:lambda

   #:cons
   #:car
   #:cdr
   #:first
   #:rest
   #:list
   #:list*

   #:caar #:cddr #:cadr #:cdar

   #:eval
   #:apply
   #:funcall

   #:push
   #:pop

   #:import

   #:make)
  (:export
   :def :define-values
   :defalias :defsubst
   :defconst :define-constant
   :make)
  (:import-from :serapeum :batches :mapply)
  (:use-reexport :cl)
  (:documentation "Just like CL, except that some forms are shadowed
  so they can be rebound."))

(in-package :vernacular/shadows)

(cl:defmacro defmacro (name args &body body)
  `(cl:defmacro ,name ,args ,@body))

(defmacro progn (&body body)
  `(cl:progn ,@body))

(defmacro locally (&body body)
  `(cl:locally ,@body))

(defmacro lambda (args &body body)
  `(cl:lambda ,args ,@body))

(cl:macrolet ((binder (ours theirs)
                (assert (not (eql ours theirs)))
                `(defmacro ,ours (binds &body body)
                   (cl:list* ',theirs binds body))))
  (binder let cl:let)
  (binder let* cl:let*)
  (binder flet cl:flet)
  (binder labels cl:labels)
  (binder macrolet cl:macrolet)
  (binder symbol-macrolet cl:symbol-macrolet))

(defmacro defun (name args &body body)
  `(cl:defun ,name ,args ,@body))

(defmacro defsubst (name args &body body)
  `(serapeum:defsubst ,name ,args
     ,@body))

(defmacro defalias (name val)
  `(serapeum:defalias ,name ,val))

(defmacro def (var expr) `(serapeum:def ,var ,expr))
(defmacro define-values (vars expr) `(serapeum:define-values ,vars ,expr))
(defmacro defconst (var expr) `(serapeum:defconst ,var ,expr))
(defmacro defconstant (var expr) `(cl:defconstant ,var ,expr))
(defmacro define-symbol-macro (var form) `(cl:define-symbol-macro ,var ,form))

(defmacro define-constant (name init &key (test ''eql) documentation)
  `(alexandria:define-constant ,name ,init
     :test ,test
     :documentation ,documentation))

(deftype cons (&optional x y)
  `(cl:cons ,x ,y))

(defsubst cons  (x y) (cl:cons x y))
(defsubst car   (x)   (cl:car x))
(defsubst first (x)   (cl:first x))
(defsubst cdr   (x)   (cl:cdr x))
(defsubst rest  (x)   (cl:rest x))

(defsubst set-car! (x val) (setf (cl:car x) val))
(defsubst set-cdr! (x val) (setf (cl:cdr x) val))

(defsetf car   set-car!)
(defsetf first set-car!)
(defsetf cdr   set-cdr!)
(defsetf rest  set-cdr!)

(defmacro define-cxr (name &rest path)
  `(progn
     (defsubst ,name (x)
       ,(reduce #'cl:list path :initial-value 'x :from-end t))
     (defsetf name (x) (v)
       (let ((acc (reduce #'cl:list ',path :initial-value x :from-end t)))
         `(setf ,acc ,v)))))

(define-cxr caar car car)
(define-cxr cddr cdr cdr)
(define-cxr cadr car cdr)
(define-cxr cdar cdr car)

(deftype list ()
  'cl:list)

(defsubst apply (function &rest args)
  (cl:apply #'cl:apply function args))

(defsubst list (&rest args)
  (apply #'cl:list args))

(defsubst list* (&rest args)
  (apply #'cl:list* args))

(defsubst eval (form)
  (cl:eval form))

(defsubst funcall (function &rest args)
  (cl:apply #'cl:funcall function args))

(defmacro push (value place)
  `(cl:push ,value ,place))

(defmacro pop (place)
  `(cl:pop ,place))

(defun import (sym-or-syms &optional (package *package*))
  (cl:import sym-or-syms package))

(defsubst make (class &rest initargs &key &allow-other-keys)
  (apply #'serapeum:make class initargs))
