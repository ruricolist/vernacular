;;;; vernacular.asd

(asdf:defsystem "vernacular"
  :description "Experimental module system."
  :author "Paul M. Rodriguez <pmr@ruricolit.com>"
  :license  "MIT"
  ;; :version "0.0.1"
  :class :package-inferred-system
  :depends-on ("vernacular/all")
  :in-order-to ((test-op (test-op "vernacular/tests")))
  :perform (test-op (o c) (symbol-call :vernacular/tests :run-vernacular-tests)))
