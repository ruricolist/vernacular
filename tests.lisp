(uiop/package:define-package :vernacular/tests
    (:use :FiveAM :vernacular/import-set)
  (:mix :vernacular/shadows :serapeum :alexandria)
  (:import-from :overlord/tests :with-temp-db :touch)
  (:import-from :vernacular :with-imports :require-as
    :with-import-default :require-default)
  ;; Languages.
  (:import-from :vernacular/demo/js)
  ;; (:import-from :vernacular/lang/sweet-exp)
  (:import-from :vernacular/lang/s-exp)
  (:import-from :core-lisp)
  (:export :run-vernacular-tests))
(in-package :vernacular/tests)

(def-suite vernacular)
(in-suite vernacular)

(defun run-vernacular-tests ()
  (let ((overlord:*base* (asdf:system-relative-pathname :vernacular ""))
        (fiveam:*on-error* :debug))
    (with-temp-db ()
      (run! 'vernacular))))

;;; Utilities.
(defmacro with-imports* ((mod &rest args &key &allow-other-keys) &body body)
  "Force reload whenever run."
  `(with-imports (,mod ,@args :once nil)
     ,@body))

(test skip-shebang
  (with-input-from-string (in (fmt "#!/bin/sh~%#lang sh"))
    (vernacular/hash-lang-syntax:stream-hash-lang in)))

;;; JS demo.

(test js-demo
  (is
   (equal "moooooooooooo"
          (with-imports* (demo1 :from "demo/demo1.js" :binding (#'moo))
            (moo 5)))))

;;; Meta-languages.

(test s-exp
      (is (= 42
             (with-import-default (answer :from "tests/s-exp-test.sexp" :once nil)
               answer))))

;; (test sweet-exp
;;   (is
;;    (= 2432902008176640000
;;       (with-imports* (factorializer :from "tests/factorial.lsp" :binding (#'fact))
;;         (fact 20)))))

(test import-default-as-function
      (is (= 2432902008176640000
             (with-import-default (#'fact :from "tests/import-as-function.lsp" :once nil)
               (fact 20)))))

;;; Prefixes and renaming.

(test (party :compile-at :run-time)
  ;; Reproduces an example from the R6RS spec.
  (with-imports* (party :from "tests/party/party.lisp" :binding :all-as-functions)
    (let ((p (make-party)))
      (is (equal (pop! p) "Boom! 108"))
      (push! p (push* (make 5 5) 1))
      (is (equal (pop! p) "Boom! 24")))))

(test (grid :compile-at :run-time)
  ;; Reproduces an example from the R7RS spec.
  (with-imports* (main :from "tests/grid/main.lisp" :binding ((run
                                                               :as #'run*)))
    (let* ((sep #\Page)
           (s (with-output-to-string (*standard-output*)
                (run* sep))))
      ;; There's some bizarre edge case in SBCL that makes this come
      ;; out to 81, but I don't think it has anything to do with
      ;; Vernacular.
      (is (memq (count sep s) '(80 81)))
      (let ((frames (split-sequence sep s :remove-empty-subseqs t)))
        (is (= (length frames) 80))
        (is-true (notany #'equal frames (rest frames)))))))

;;; Import as package.

(test import-as-package
  (let ((pkg :vernacular-test/as-package))
    (when (find-package pkg)
      (delete-package pkg))
    (eval `(vernacular:import-as-package ,pkg
             :from "tests/islisp/exports.lsp"
             :binding (x #'y (macro-function z))))
    (is-true (find-package pkg))
    (is (equal '(:var :fn :macro)
               (eval `(list ,(find-symbol (string 'x) pkg)
                            (,(find-symbol (string 'y) pkg))
                            (,(find-symbol (string 'z) pkg))))))))

;;; Import sets.

(def-suite import-set :in vernacular)

(in-suite import-set)

(test all
  (is (null (expand-import-set :all (constantly nil))))

  (is (import-set=
       '(x)
       (expand-import-set :all (constantly '(:x)))))

  (is (import-set=
       '(#'x)
       (expand-import-set :all-as-functions
                          (constantly '(:x))))))

(test only
  (is (null
       (expand-import-set '(:only :all)
                          (constantly nil))))

  (is (null
       (expand-import-set '(:only :all)
                          (constantly '(:x)))))

  (is (import-set=
       '(x)
       (expand-import-set '(:only :all x)
                          (constantly '(:x :y :z)))))

  (is (import-set=
       '(#'x)
       (expand-import-set '(:only :all-as-functions #'x)
                          (constantly '(:x :y :z)))))

  ;; Id not present.
  (signals error
    (expand-import-set '(:only :all x)
                       (constantly nil)))

  (signals error
    (expand-import-set '(:only :all x)
                       (constantly '(:a))))

  ;; Importing a var as a function.
  (signals error
    (expand-import-set '(:only :all-as-functions x)
                       (constantly '(:x :y :z))))

  ;; Importing a function as a var.
  (signals error
    (expand-import-set '(:only :all #'x)
                       (constantly '(:x :y :z)))))

(test except
  (is (null
       (expand-import-set '(:except :all)
                          (constantly nil))))

  (is (import-set=
       '(x)
       (expand-import-set '(:except :all)
                          (constantly '(:x)))))

  (is (import-set=
       '(y z)
       (expand-import-set '(:except :all x)
                          (constantly '(:x :y :z)))))

  (is (import-set=
       '(#'y #'z)
       (expand-import-set '(:except :all-as-functions #'x)
                          (constantly '(:x :y :z)))))

  (signals error
    (expand-import-set '(:except :all x)
                       (constantly nil)))

  (signals error
    (expand-import-set '(:except :all x)
                       (constantly :a))))

(test prefix
  (is (null
       (expand-import-set '(:prefix :all my-)
                          (constantly nil))))

  (is (import-set= '(my-x my-y my-z)
                   (expand-import-set '(:prefix :all my-)
                                      (constantly '(:x :y :z)))))

  (is (import-set= '(nilx nily nilz)
                   (expand-import-set '(:prefix :all nil)
                                      (constantly '(:x :y :z)))))

  (is (import-set= '(#'my-x #'my-y #'my-z)
                   (expand-import-set '(:prefix :all-as-functions my-)
                                      (constantly '(:x :y :z))))))

(test drop-prefix
  (is (null
       (expand-import-set '(:drop-prefix :all my-)
                          (constantly nil))))

  (is (import-set=
       '(x y z)
       (expand-import-set '(:drop-prefix :all my-)
                          (constantly '(:my-x :my-y :my-z)))))

  (is (import-set=
       '(x y z)
       (expand-import-set '(:drop-prefix :all nil)
                          (constantly '(:nilx :nily :nilz)))))

  (is (import-set=
       '(#'x #'y #'z)
       (expand-import-set '(:drop-prefix :all-as-functions my-)
                          (constantly '(:my-x :my-y :my-z))))))

(test rename
  (is (null
       (expand-import-set '(:rename :all)
                          (constantly nil))))

  (is (import-set=
       '(x)
       (expand-import-set '(:rename :all)
                          (constantly '(:x)))))

  (is (import-set=
       '(a b c)
       (expand-import-set '(:rename :all (x a) (y b) (z c))
                          (constantly '(:x :y :z)))))

  (signals error
    (expand-import-set '(:rename :all-as-functions (x a) (y b) (z c))
                       (constantly '(:x :y :z))))

  (is (import-set=
       '(#'a #'b #'c)
       (expand-import-set '(:rename :all-as-functions
                            (#'x #'a) (#'y #'b) (#'z #'c))
                          (constantly '(:x :y :z)))))

  (signals error
    (expand-import-set '(:rename :all
                         (#'x #'a) (#'y #'b) (#'z #'c))
                       (constantly '(:x :y :z)))))

(test alias
  (is (null
       (expand-import-set '(:alias :all)
                          (constantly nil))))

  (is (import-set=
       '(x)
       (expand-import-set '(:alias :all)
                          (constantly '(:x)))))

  (is (import-set=
       '(x y z a b c)
       (expand-import-set '(:alias :all (x a) (y b) (z c))
                          (constantly '(:x :y :z)))))

  (signals error
    (expand-import-set '(:alias :all-as-functions (x a) (y b) (z c))
                       (constantly '(:x :y :z))))

  (is (import-set=
       '(#'a #'b #'c #'x #'y #'z)
       (expand-import-set '(:alias :all-as-functions
                            (#'x #'a) (#'y #'b) (#'z #'c))
                          (constantly '(:x :y :z)))))

  (signals error
    (expand-import-set '(:alias :all
                         (#'x #'a) (#'y #'b) (#'z #'c))
                       (constantly '(:x :y :z)))))


;;; Core Lisp.

(def-suite islisp :in vernacular)

(in-suite islisp)

(test hello-islisp
  (is (equal "hello world"
             (with-imports* (m :from "tests/islisp/islisp.lsp" :binding (hello))
               hello))))

(test islisp-dont-be-shadowed
  (is (equal '(:right :right :right)
             (with-imports* (m :from "tests/islisp/dont-be-shadowed.lsp"
                               :binding (syms (xyz :as #'expand-xyz)))
               (destructuring-bind (x y z) syms
                 (eval
                  `(let ((,x :wrong)) (declare (ignorable ,x))
                     (flet ((,y () :wrong)) (declare (ignore #',y))
                       (macrolet ((,z () :wrong))
                         ,(expand-xyz nil nil))))))))))

;;; TODO.
;; (test islisp-imports
;;   (is (equal '(:var :fn :macro)
;;              (require-default "tests/islisp/imports.lsp"))))

(test islisp-auto-alias
  (is (equal '(0 1)
             (require-default "tests/islisp/shadowing.lsp"))))

(test islisp-hygiene
  (touch #1="tests/islisp/hygiene.lsp")
  ;; Not the desired results, just the ones we expect.
  (handler-bind ((warning #'muffle-warning))
    (is (equal '(4 6 :ERROR 4 16 :ERROR)
               (require-default #1#)))))

(test islisp-globals-can-close
  "Test that globals defined with `defglobal' close over themselves."
  (with-imports* (m :from "tests/islisp/globals-can-close.lsp" :binding (x))
    (is (eql x (funcall x)))))

(test islisp-phasing
  "Test that state is not preserved across rebuilds."
  (require-as nil #1="tests/islisp/phasing.lsp")
  (with-imports* (m :from #1# :binding (#'inc-count))
    (is (= (inc-count) 0))))
