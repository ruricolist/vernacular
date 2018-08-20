#lang vernacular/simple-module

(:import life :from "life.lisp"
  :binding (#'life))
(:export #'run)

;;; TODO
;; (rename (prefix (example grid) grid-)
;;         (grid-make make-grid))

(:import grid :from "grid.lisp"
         :binding
         (:import-set
          (:rename
           (:prefix :all-as-functions grid-)
           (#'grid-make #'make-grid))))

;;; Initialize a grid with a glider.
(def grid (make-grid 24 24))
(grid-set! grid 1 1 t)
(grid-set! grid 2 2 t)
(grid-set! grid 3 0 t)
(grid-set! grid 3 1 t)
(grid-set! grid 3 2 t)

(defun run (&optional (separator #\Page))
  (life grid 80 separator))
