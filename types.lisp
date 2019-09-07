(defpackage :vernacular/types
  (:documentation "Types used throughout.")
  (:use :cl :alexandria :serapeum :overlord/types)
  (:import-from :trivia :defpattern :ematch :match)
  (:export
   #:vernacular-error
   ;; Imports and exports.
   #:import-alias
   #:bindable-symbol
   #:non-keyword
   #:ns
   #:function-spec
   #:public-side
   #:private-side
   #:public-name
   #:public-ns
   #:private-name
   #:private-ns))
(in-package :vernacular/types)

(defcondition vernacular-error (overlord-error)
  ())

(defun vernacular-error (format-control &rest format-arguments)
  (error 'vernacular-error
         :format-control format-control
         :format-arguments format-arguments))

(define-compiler-macro vernacular-error (&whole call
                                                format-control &rest format-arguments)
  (if (stringp format-control)
      `(vernacular-error (formatter ,format-control)
                         ,@format-arguments)
      call))

(defconst cl-constants
  (collecting
    (do-external-symbols (s :cl)
      (when (constantp s)
        (collect s)))))

(deftype bindable-symbol ()
  "To a rough approximation, a symbol that can/should be bound."
  '(and symbol
    (not (member nil t function quote))
    (not keyword)))

(deftype non-keyword ()
  `(and symbol
        (not keyword)
        ;; XXX Too slow.
        #+(or) (not (satisfies constantp))
        (not (member ,@cl-constants))
        ;; This would just be confusing.
        (not (member quote function))))

(deftype ns ()
  '(member nil function macro-function setf))

(defpattern ns (ns sym)
  `(list (and ,ns (type ns))
         ,sym))

;;; "Function spec" is Allegro's name for a list as a function name.

(defpattern function-spec (ns &rest syms)
  `(list 'function
         (list (and ,ns (type symbol))
               ,@syms)))

(defun ns+name (spec)
  (ematch spec
    ((and sym (type symbol))
     (values nil sym))
    ((function-spec ns sym)
     (values ns sym))
    ((ns ns sym)
     (values ns sym))))

(defun public-side (clause)
  (ematch clause
    ((type symbol) clause)
    ((function-spec ns name)
     (list ns name))
    ((ns nil name) name)
    ((ns _ _) clause)
    ((list _ :as public)
     (public-side public))))

(defun private-side (clause)
  (ematch clause
    ((type symbol) clause)
    ((function-spec _ _)
     clause)
    ((ns nil name) name)
    ;; The private side should be something that can be evaluated.
    ((ns 'setf name)
     `#'(function (setf ,name)))
    ((ns _ _) clause)
    ((list private :as _)
     (private-side private))))

(defun public-name (clause)
  (assure symbol
    (nth-value 1 (ns+name (public-side clause)))))

(defun public-ns (clause)
  (assure ns
    (nth-value 0 (ns+name (public-side clause)))))

(defun private-name (spec)
  (assure symbol
    (nth-value 1 (ns+name (private-side spec)))))

(defun private-ns (spec)
  (assure ns
    (nth-value 0 (ns+name (private-side spec)))))
