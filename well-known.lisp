(defpackage :vernacular/well-known
  (:documentation "Symbols from this package are used in a keyword-like manner.")
  (:use)
  (:export :default :main))

#+sb-package-locks
(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-ext:lock-package (find-package :vernacular/well-known)))
