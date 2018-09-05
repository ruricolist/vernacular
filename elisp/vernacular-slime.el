;;; vernacular.el -*- lexical-binding: t -*-

(require 'vernacular-common)
(require 'slime)

(defun vernacular-eval (form)
  (slime-eval-with-transcript form))

(defun vernacular-lisp-file-name (file)
  (slime-to-lisp-filename file))

(defun vernacular-switch-repl ()
  (interactive)
  (call-interactively 'slime-switch-to-output-buffer))

(defun vernacular-expand-module ()
  (interactive)
  (let* ((expr (vernacular--expand-module-expr)))
    (slime-eval-describe
     `(swank::swank-pprint
       (cl:list ,(prin1-to-string expr))))))

(provide 'vernacular-slime)
