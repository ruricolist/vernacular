;;;; package.lisp

(uiop:define-package :vernacular/all
  (:nicknames :vernacular)
  (:import-from :overlord)
  (:use :overlord/target-protocol)
  (:import-from :vernacular/module
    :module-ref :module-ref* :module-ref-ns :module-exports :module-static-exports)
  (:import-from :vernacular/simple-module
    :simple-module)
  (:import-from :vernacular/hash-table-module
    :hash-table-module)
  (:import-from :vernacular/file-package
    :intern-file-package
    :reset-file-package)
  (:import-from :vernacular/specials
    :*module* :*source*)
  (:export
   :module-ref :module-ref* :module-ref-ns :module-exports :module-static-exports
   :simple-module :hash-table-module :package-module
   :intern-file-package :reset-file-package
   :*module* :*source*)
  (:use-reexport
   :vernacular/lang
   :vernacular/import-set
   :vernacular/importing
   :vernacular/parsers
   :vernacular/include))
