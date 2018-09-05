;;; vernacular.el -*- lexical-binding: t -*-

(require 'vernacular-common)
(require 'sly)

(defun vernacular-eval (form)
  (sly-eval-with-transcript form))

(defun vernacular-lisp-file-name (file)
  (sly-to-lisp-filename file))

(defun vernacular-switch-repl ()
  (interactive)
  (call-interactively 'sly-mrepl))

(defun vernacular-expand-module ()
  (interactive)
  (let* ((expr (vernacular--expand-modulex-expr)))
    ;; TODO Prettier output (more like `sly-expand-1').
    (sly-eval-describe
     `(slynk:pprint-eval ,(prin1-to-string expr)))))

(provide 'vernacular-sly)
