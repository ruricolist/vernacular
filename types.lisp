(defpackage :vernacular/types
  (:documentation "Types used throughout.")
  (:use :cl :alexandria :serapeum :overlord/types)
  (:import-from :trivia :defpattern)
  (:export
   #:vernacular-error
   ;; Imports and exports.
   #:import-alias
   #:bindable-symbol
   #:export-alias
   #:var-spec
   #:function-spec
   #:macro-spec
   #:binding-spec
   #:export-spec
   #:var-alias
   #:function-alias
   #:macro-alias
   #:binding-designator
   #:canonical-binding
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

(deftype ns ()
  '(member nil function macro-function setf))

(defpattern ns (ns sym)
  `(list (and ,ns (type ns))
         (and ,sym (type symbol))))

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

(deftype var-spec ()
  'non-keyword)

(deftype function-spec ()
  '(tuple 'function bindable-symbol))

(deftype macro-spec ()
  '(tuple 'macro-function bindable-symbol))

;;; Exports.

(deftype export-alias ()
  '(and symbol (not (member t nil function quote))))

(deftype export-spec ()
  '(or var-spec
    function-spec
    macro-spec
    (tuple var-spec :as export-alias)
    (tuple function-spec :as export-alias)
    (tuple macro-spec :as export-alias)))

;;; Imports.

(deftype var-alias () 'bindable-symbol)
(deftype function-alias () '(tuple 'function bindable-symbol))
(deftype macro-alias () '(tuple 'macro-function bindable-symbol))
(deftype import-alias () '(or var-alias function-alias macro-alias))

(deftype binding-spec ()
  '(or (member :all :all-as-functions)
    (tuple :import-set list)
    list))

(deftype canonical-binding ()
  '(tuple keyword import-alias))

(deftype binding-designator ()
  '(or
    var-spec
    function-spec
    macro-spec
    (tuple symbol :as import-alias)))
