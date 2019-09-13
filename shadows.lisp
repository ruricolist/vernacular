(uiop:define-package :vernacular/shadows
  (:import-from :alexandria)
  (:import-from :trivia :defpattern)
  (:nicknames :vernacular/cl)
  (:shadow
   #:module-progn

   #:defun
   #:defmacro
   #:defconstant
   #:define-symbol-macro
   #:deftype
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

   #:make

   #:t #:otherwise
   ;; In order to shadow T, we also need to shadow these.
   #:cond
   #:case #:ecase #:ccase
   #:typecase #:etypecase #:ctypecase)
  (:export
   :def :define-values
   :defalias :defsubst
   :defconst :define-constant
   :make)
  (:import-from :serapeum :batches :mapply :eval-always)
  (:use-reexport :cl)
  (:documentation "Just like CL, except that some forms are shadowed
  so they can be rebound."))

(in-package :vernacular/shadows)

#-ccl
(setf (find-class 't) (find-class 'cl:t))

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
(defmacro deftype (name lambda-list &body body) `(cl:deftype ,name ,lambda-list ,@body))

(defmacro define-constant (name init &key (test ''eql) documentation)
  `(alexandria:define-constant ,name ,init
     :test ,test
     :documentation ,documentation))

;; Shadowing `t' isn't quite as easy as it looks. If you make it a
;; constant -- either as a true constant, or as a symbol macro -- SBCL
;; won't let you use it as a slot name. But if it has to be evaluated,
;; it confuses SBCL's type inference when it comes to `cond' and the like. We
;; compromise by shadowing `cond' as well.

(define-symbol-macro t cl:t)

#+(or sbcl cmucl)
(eval-always
  (def t cl:t))

(eval-always
  ;; CCL objects to redefining.
  (unless (alexandria:type= t 'cl:t)
    (deftype t () 'cl:t)))

(setf (find-class 't) (find-class 'cl:t))

(defun handle-default-keys (clauses)
  (loop for clause in clauses
        collect (cons
                 (cl:case (car clause)
                   ((t) 'cl:t)
                   ((otherwise) 'cl:otherwise)
                   (cl:t (car clause)))
                 (cdr clause))))

(defmacro cond (&rest clauses) `(cl:cond ,@(handle-default-keys clauses)))

(defmacro shadow-case (name)
  (let ((cl-name (find-symbol (string name) :cl)))
    `(defmacro ,name (keyform &rest clauses)
       (list* ',cl-name keyform (handle-default-keys clauses)))))

(defmacro shadow-cases (&body names)
  `(progn
     ,@(loop for name in names
             collect `(shadow-case ,name))))

(shadow-cases
  case ecase ccase
  typecase etypecase ctypecase)

(deftype cons (&optional x y)
  `(cl:cons ,x ,y))

(defpattern cons (car cdr)
  `(cl:cons ,car ,cdr))

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

(defpattern list (&rest args)
  `(cl:list ,@args))

(defsubst apply (function &rest args)
  (cl:apply #'cl:apply function args))

(defsubst list (&rest args)
  (apply #'cl:list args))

(defsubst list* (&rest args)
  (apply #'cl:list* args))

(defpattern list* (&rest args)
  `(cl:list ,@args))

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
