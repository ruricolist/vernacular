(defpackage :vernacular/types
  (:documentation "Types used throughout.")
  (:use :cl :alexandria :serapeum :overlord/types)
  (:import-from :trivia :defpattern)
  (:export
   #:vernacular-error
   ;; Imports and exports.
   #:import-alias
   #:bindable-symbol
   #:non-keyword
   #:ns))
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
         (and ,sym (type symbol))))
