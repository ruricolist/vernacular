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

(defun rename-import (import new-name)
  `(,(private-side import)
    :as
    (,(public-ns new-name)
     ,(public-name new-name))))

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
                  (:all-as-setters
                   (loop for export in (get-exports)
                         for sym = (intern (string export) package)
                         collect `(#'(setf ,export) :as #'(setf ,sym))))
                  ((list* :only import-set ids)
                   (only (rec import-set) ids))
                  ((list* :except import-set ids)
                   (except (rec import-set) ids))
                  ;; Renames are a list of (old-name new-name).
                  ((list* :rename import-set renames)
                   (rename (rec import-set) renames))
                  ((list :prefix import-set prefix)
                   (prefix (rec import-set) prefix))
                  ;; Same thing as :prefix.
                  ((list :add-prefix import-set prefix)
                   (rec `(:prefix ,import-set ,prefix)))
                  ((list :drop-prefix import-set prefix)
                   (drop-prefix (rec import-set) prefix))
                  ;; Alias is just like rename, except that the old
                  ;; binding isn't removed.
                  ((list* :alias import-set renames)
                   (alias (rec import-set) renames))))))
    (nub (rec import-set))))

(defun only (import-set ids)
  (reduce (lambda (out id)
            (if-let (import (find-id id import-set))
              (cons import out)
              (missing-id id import-set)))
          ids
          :initial-value nil))

(defun except (import-set ids)
  (reduce (lambda (import-set id)
            (if-let (import (find-id id import-set))
              (remove import import-set)
              (missing-id id import-set)))
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
                (missing-id old-name import-set))))
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

(defcondition import-set-condition ()
  ())

(defcondition import-set-error (import-set-condition simple-error)
  ())

(defun missing-id (id import-set)
  (error 'import-set-error
         :format-control "Missing id ~a in import set ~s"
         :format-arguments (list id import-set)))

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
