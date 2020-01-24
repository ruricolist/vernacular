;;;; vernacular.asd

(defsystem "vernacular"
  :description "Module system for language embeddings."
  :author "Paul M. Rodriguez <pmr@ruricolit.com>"
  :license  "MIT"
  :version "0.8.0"
  :class :package-inferred-system
  :depends-on ("vernacular/all")
  :in-order-to ((test-op (test-op "vernacular/tests")))
  :perform (test-op (o c) (symbol-call :vernacular/tests :run-vernacular-tests)))
