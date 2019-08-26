(defpackage :vernacular/import-set
  (:documentation "Syntax for import sets.
Influenced by, but not identical with, the R6RS syntax.")
  (:use :cl :alexandria :serapeum
    :vernacular/types)
  (:import-from :trivia :ematch)
  (:export
   :expand-import-set
   :import-set=))
(in-package :vernacular/import-set)

(defun public-name+private-name (import)
  (receive (public private)
      (ematch import
        ((type symbol)
         (values import import))
        ((ns _ private)
         (values import private))
        ((list private :as public)
         (values (public-name public)
                 (private-name private))))
    (values public
            (make-keyword private))))

(defun public-name (import)
  (nth-value 0 (public-name+private-name import)))

(defun private-name (import)
  (nth-value 1 (public-name+private-name import)))

(defun public-name= (x y)
  (equal x y))

(defun rename-import (import new-name)
  (assure binding-designator
    (list (private-name import) :as new-name)))

(defun expand-import-set (import-set get-exports
                          &optional (package *package*))
  (unless (functionp get-exports)
    (setf get-exports (constantly get-exports)))
  (fbindrec (get-exports
             (rec
              (lambda (import-set)
                (ematch import-set
                  (:all
                   (loop for export in (get-exports)
                         for sym = (intern (string export) package)
                         collect `(,export :as ,sym)))
                  (:all-as-functions
                   (loop for export in (get-exports)
                         for sym = (intern (string export) package)
                         collect `(#',export :as #',sym)))
                  ((list* :only import-set ids)
                   (only (rec import-set) ids))
                  ((list* :except import-set ids)
                   (except (rec import-set) ids))
                  ((list* :rename import-set renames)
                   (rename (rec import-set) renames))
                  ((list :prefix import-set prefix)
                   (prefix (rec import-set) prefix))
                  ((list :drop-prefix import-set prefix)
                   (drop-prefix (rec import-set) prefix))
                  ((list* :alias import-set renames)
                   (alias (rec import-set) renames))))))
    (rec import-set)))

(defun only (import-set ids)
  (reduce (lambda (out id)
            (if-let (import (find id import-set :test #'named?))
              (cons import out)
              (missing-id id import-set)))
          ids
          :initial-value nil))

(defun except (import-set ids)
  (reduce (lambda (import-set id)
            (if-let (import (find id import-set :test #'named?))
              (remove import import-set)
              (missing-id id import-set)))
          ids
          :initial-value import-set))

(defun rename (import-set renames)
  (reduce (lambda (import-set renaming)
            (receive (old-name new-name) (old-name+new-name renaming)
              (if-let (import (find old-name import-set :test #'named?))
                (cons (rename-import import new-name)
                      (remove import import-set))
                (missing-id old-name import-set))))
          renames
          :initial-value import-set))

(defun alias (import-set renames)
  (reduce (lambda (import-set renaming)
            (receive (old-name new-name) (old-name+new-name renaming)
              (if-let (import (find old-name import-set :test #'named?))
                (cons (rename-import import new-name)
                      import-set)
                (missing-id old-name import-set))))
          renames
          :initial-value import-set))

(defun old-name+new-name (renaming)
  (destructuring-bind (old-name new-name) renaming
    (values old-name new-name)))

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
        for public-name = (public-name import)
        for new-name = (frob-name public-name rename-fn)
        collect (rename-import import new-name)))

(defun frob-name (name fn)
  (fbind (fn)
    (ematch name
      ((type symbol) (fn name))
      ((list (and ns (type symbol))
             name)
       `(,ns ,(fn name))))))

(defcondition import-set-condition ()
  ())

(defcondition import-set-error (import-set-condition simple-error)
  ())

(defun missing-id (id import-set)
  (error 'import-set-error
         :format-control "Missing id ~a in import set ~s"
         :format-arguments (list id import-set)))

(defun named? (name import)
  (public-name= name (public-name import)))

(defun import-set= (set1 set2)
  (set-equal set1 set2
             :key #'public-name
             :test #'public-name=))
