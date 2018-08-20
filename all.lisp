;;;; package.lisp

(uiop:define-package :vernacular/all
    (:nicknames :vernacular)
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
  (:export
   :module-ref :module-ref* :module-exports :module-static-exports
   :simple-module :hash-table-module
   :ensure-file-package :reset-file-package)
  (:use-reexport
   :overlord/lang
   :overlord/import-set
   :overlord/importing))
