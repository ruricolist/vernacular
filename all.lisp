;;;; package.lisp

(uiop:define-package :vernacular/all
    (:nicknames :vernacular)
  (:import-from :overlord)
  (:use :overlord/target-protocol)
  (:import-from :vernacular/module
    :module-ref :module-ref* :module-exports :module-static-exports)
  (:import-from :vernacular/simple-module
    :simple-module)
  (:import-from :vernacular/hash-table-module
    :hash-table-module)
  (:import-from :vernacular/file-package
    :ensure-file-package
    :reset-file-package)
  (:import-from :vernacular/specials)
  (:export
   :module-ref :module-ref* :module-exports :module-static-exports
   :simple-module :hash-table-module
   :ensure-file-package :reset-file-package)
  (:use-reexport
   :vernacular/lang
   :vernacular/importing
   :vernacular/parsers))
