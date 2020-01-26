(defpackage :vernacular/import-set
  (:documentation "Syntax for import sets.
Influenced by, but not identical with, the R6RS syntax.")
  (:use :cl :alexandria :serapeum
    :vernacular/types)
  (:import-from :trivia :match :ematch)
  (:export
   :expand-import-set
   :import-set=))
(in-package :vernacular/import-set)

(defcondition import-set-condition ()
  ())

(defcondition import-set-error (import-set-condition simple-error)
  ((import-set :initarg :import-set)))

(defcondition invalid-import-set (import-set-error)
  ()
  (:report (lambda (c s)
             (with-slots (import-set) c
               (format s "Invalid import set:~%~s" import-set)))))

(defcondition missing-id (import-set-error)
  ((id :initarg :id))
  (:report (lambda (c s)
             (with-slots (id import-set) c
               (format s "Missing id ~s in import set:~%~s" id import-set)))))

(defun rename-import (import new-name)
  `(,(private-side import)
    :as
    (,(public-ns new-name)
     ,(public-name new-name))))

(defun same-binding? (x y)
  (and (equal (public-side x) (public-side y))
       (equal (private-side x) (private-side y))))

(defun expand-import-set (import-set get-exports
                          &key (package *package*))
  "Expand IMPORT-SET, an R6RS-style import set, into a list of imports.
Imports are aliased to ensure that they are accessible in the current
package (or PACKAGE, if specified.)"
  (unless (functionp get-exports)
    (setf get-exports (constantly get-exports)))
  (fbindrec ((get-exports
              (once
               (lambda ()
                 (let ((exports (funcall get-exports)))
                   ;; Strip reserved names (default, main, etc.).
                   (remove (find-package :vernacular/well-known)
                           exports
                           :key (compose #'symbol-package #'public-name))))))
             (local-name
              (lambda (export)
                (intern (string (public-name export)) package)))
             (rec
              (lambda (import-set)
                (match import-set
                  ;; Compat.
                  ((list* :import-set import-sets)
                   (mappend #'rec import-sets))
                  ;; Coerce everything to a variable
                  (:all
                   (loop for export in (get-exports)
                         for sym = (local-name export)
                         collect `(,export :as ,sym)))
                  ;; Coerce everything to a function.
                  (:all-as-functions
                   (loop for export in (get-exports)
                         for sym = (local-name export)
                         collect `(,(public-side export) :as #',sym)))
                  (:all-as-setters
                   ;; Coerce everything to a setf function.
                   (loop for export in (get-exports)
                         for sym = (local-name export)
                         collect `(,(public-side export) :as #'(setf ,sym))))
                  ;; Just marked variables.
                  (:all-vars
                   (loop for export in (get-exports)
                         when (eql (public-ns export) 'nil)
                           collect `(,(public-side export) :as ,(local-name export))))
                  ;; Just marked setf functions.
                  (:all-setters
                   (loop for export in (get-exports)
                         when (eql (public-ns export) 'setf)
                           collect `(,(public-side export) :as #'(setf ,(local-name export)))))
                  ;; All functions, plus any setf functions with the same names.
                  (:all-functions
                   (let* ((functions (keep 'function (get-exports) :key #'public-ns))
                          (setters (keep 'setf (get-exports) :key #'public-ns))
                          (function-setters
                            (filter (lambda (setter)
                                      (find (public-name setter)
                                            functions
                                            :key #'public-name))
                                    setters)))
                     (rec (append functions function-setters))))
                  ;; Remove everything but IDs from IMPORT-SET.
                  ((list* :only import-set ids)
                   (only (rec import-set) ids))
                  ;; Remove IDs from IMPORT-SET.
                  ((list* :except import-set ids)
                   (except (rec import-set) ids))
                  ;; Renames are a list of (old-name new-name).
                  ((list* :rename import-set renames)
                   (rename (rec import-set) renames))
                  ;; Add PREFIX to every local name in IMPORT-SET.
                  ((list :prefix import-set prefix)
                   (prefix (rec import-set) prefix))
                  ;; Same thing as :prefix.
                  ((list :add-prefix import-set prefix)
                   (rec `(:prefix ,import-set ,prefix)))
                  ;; Remove PREFIX from every local name in IMPORT-SET.
                  ((list :drop-prefix import-set prefix)
                   (drop-prefix (rec import-set) prefix))
                  ;; Alias is just like rename, except that the old
                  ;; binding isn't removed.
                  ((list* :alias import-set renames)
                   (alias (rec import-set) renames))
                  ((type keyword)
                   (error 'invalid-import-set
                          :import-set import-set))
                  ;; Imports per se.
                  ((and sym (type symbol))
                   (list `(,sym :as ,(local-name sym))))
                  ((function-spec ns symbol)
                   (list `((function (,ns ,symbol)) :as (function (,ns ,(local-name symbol))))))
                  ((ns ns symbol)
                   (list `((,ns ,symbol) :as (,ns ,(local-name symbol)))))
                  ;; Leave explicit aliases unchanged.
                  ((and export (list _ :as _)) (list export))
                  ;; Descend into lists.
                  ((type list)
                   (mappend #'rec import-set))
                  (otherwise
                   (error 'invalid-import-set
                          :import-set import-set))))))
    (remove-duplicates (rec import-set) :test #'same-binding?)))

(defun only (import-set ids)
  (reduce (lambda (out id)
            (if-let (import (find-id id import-set))
              (cons import out)
              (error 'missing-id
                     :id id
                     :import-set import-set)))
          ids
          :initial-value nil))

(defun except (import-set ids)
  (reduce (lambda (import-set id)
            (if-let (import (find-id id import-set))
              (remove import import-set)
              (error 'missing-id
                     :id id
                     :import-set import-set)))
          ids
          :initial-value import-set))

(defun rename (import-set renames)
  (rename-alias-common import-set renames t))

(defun alias (import-set renames)
  (rename-alias-common import-set renames nil))

(defun rename-alias-common (import-set renames remove?)
  (reduce (lambda (import-set renaming)
            (destructuring-bind (old-name new-name) renaming
              (if-let (import (find-id old-name import-set))
                (cons (rename-import import new-name)
                      (if remove?
                          (remove import import-set)
                          import-set))
                (error 'missing-id
                       :id old-name
                       :import-set import-set))))
          renames
          :initial-value import-set))

(defun prefix (import-set prefix)
  (mass-rename import-set (op (symbolicate prefix _))))

(defun drop-prefix (import-set prefix)
  (flet ((drop-prefix (symbol)
           (eif (string^= prefix symbol)
               (intern (drop (length (string prefix))
                             (symbol-name symbol))
                       (symbol-package symbol))
               symbol)))
    (mass-rename import-set #'drop-prefix)))

(defun mass-rename (import-set rename-fn)
  (loop for import in import-set
        for new-name = (frob-name (public-side import) rename-fn)
        collect (rename-import import new-name)))

(defun frob-name (spec frob)
  (fbind (frob)
    `(,(public-ns spec)
      ,(frob (public-name spec)))))

(defun find-id (id imports)
  (find (public-side id)
        imports
        :key #'public-side
        :test #'equal))

(defun import-set= (set1 set2)
  (set-equal set1 set2
             :key #'public-side
             :test (lambda (x y)
                     (let ((x
                             (match x
                               ((list nil x) x)
                               (t x)))
                           (y
                             (match y
                               ((list nil y) y)
                               (t y))))
                       (equal x y)))))
