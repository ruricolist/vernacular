(defpackage #:vernacular/parsers
  (:use #:cl #:alexandria #:serapeum)
  (:import-from #:named-readtables
    #:find-readtable)
  (:import-from :vernacular/hash-lang-syntax :skip-hash-lang)
  (:export #:slurp-stream #:slurp-file)
  (:documentation "Trivial parsers."))
(in-package #:vernacular/parsers)

(defconst eof "eof")

(defun slurp-stream (in
                     &key (readtable :standard)
                          (package *package*)
                          (read-eval nil)
                          (read-base 10)
                          (read-default-float-format 'single-float))
  (skip-hash-lang in)
  (with-standard-io-syntax
    (let ((*readtable*                 (find-readtable readtable))
          (*package*                   (find-package package))
          (*read-eval*                 read-eval)
          (*read-base*                 read-base)
          (*read-default-float-format* read-default-float-format))
      (loop for form = (read in nil eof)
            until (eq form eof)
            collect form))))

(defun slurp-file (file &rest args &key &allow-other-keys)
  (with-input-from-file (in file)
    (apply #'slurp-stream in args)))
