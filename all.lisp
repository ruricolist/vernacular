;;;; package.lisp

(uiop:define-package :vernacular/all
    (:nicknames :vernacular)
  (:use :overlord/target-protocol)
  (:import-from :overlord/module
    :module-ref :module-ref* :module-exports :module-static-exports)
  (:import-from :overlord/simple-module
    :simple-module)
  (:import-from :overlord/hash-table-module
    :hash-table-module)
  (:import-from :overlord/file-package
    :ensure-file-package
    :reset-file-package)
  (:export
   :module-ref :module-ref* :module-exports :module-static-exports
   :simple-module :hash-table-module
   :ensure-file-package :reset-file-package)
  (:use-reexport
   :overlord/lang
   :overlord/importing))
