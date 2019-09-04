(defpackage :vernacular/symbols
  (:documentation "Export important symbols, without definitions.")
  (:use)
  (:export :default :main))

#+sb-package-locks
(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-ext:lock-package (find-package :vernacular/symbols)))
