(uiop:define-package :vernacular/simple-module
    (:use-reexport :overlord/simple-module)
  (:import-from :vernacular/shadows))

(defpackage :vernacular/simple-module-user
  (:use :vernacular/simple-module :vernacular/shadows))
