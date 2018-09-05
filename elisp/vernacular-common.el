;;; vernacular.el -*- lexical-binding: t -*-

(defvar vernacular-lang-hist ())

(defvar vernacular-lang nil
  "The Vernacular language to use for this buffer.
Must be the name of a CL package.")
(make-variable-buffer-local 'vernacular-lang)
(put 'vernacular-lang 'safe-local-variable 'stringp)

(defvar vernacular-module-name-hist ())

(defun vernacular-hash-lang ()
  (save-excursion
    (goto-char (point-min))
    ;; A stricter implementation would only skip comments.
    (when (re-search-forward "^#lang +\\([-/_+a-zA-Z0-9]+\\)" nil t)
      (match-string-no-properties 1))))

(defun vernacular-read-lang ()
  (completing-read "Language: "
                   vernacular-lang-hist
                   nil nil nil 'vernacular-lang-hist))

(defun vernacular-lang ()
  (or vernacular-lang
      (vernacular-hash-lang)
      (vernacular-read-lang)))

(defun vernacular-read-module-name ()
  (intern
   (completing-read "Module name: "
                    vernacular-module-name-hist
                    nil nil nil 'vernacular-module-name-hist)))

(defun vernacular-compile-file (file lang)
  (interactive (list buffer-file-name (vernacular-lang)))
  (let ((file (vernacular-lisp-file-name file)))
    (vernacular-eval
     `(vernacular:require-for-emacs ,lang ,file))))

(defun vernacular-buffer-file-name ()
  (vernacular-lisp-file-name
   (buffer-file-name)))

(defun vernacular--import (file lang binding)
  (let ((file (vernacular-lisp-file-name file))
        (module-name (vernacular-read-module-name)))
    (vernacular-eval
     `(cl:progn (vernacular:import ,module-name
                                   :from ,file
                                   :as ,lang
                                   :binding ,binding)
                ;; TODO Display a list of imports.
                ,file))))

(defun vernacular-import-vars (file lang)
  (interactive (list buffer-file-name (vernacular-lang)))
  (vernacular--import file lang ':all))

(defun vernacular-import-fns (file lang)
  (interactive (list buffer-file-name (vernacular-lang)))
  (vernacular--import file lang ':all-as-functions))

(defun vernacular-import-module (file lang)
  (interactive (list buffer-file-name (vernacular-lang)))
  (vernacular--import file lang nil))

(defun vernacular-expand-module-expr ()
  (save-some-buffers)
  (let* ((lang (vernacular-lang))
         (file (vernacular-lisp-file-name buffer-file-name)))
    `(vernacular:expand-module-for-emacs ,lang ,file)))

(defun maybe-activate-vernacular ()
  (save-excursion
    (goto-char (point-min))
    (when (looking-at-p "#lang ")
      (vernacular-mode 1))))

;;; It could be prog-mode-hook, but then we would be conflicting with
;;; Racket.
(add-hook 'lisp-mode-hook 'maybe-activate-vernacular)

(defvar vernacular-mode-keymap
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-c C-k") 'vernacular-compile-file)
    (define-key m (kbd "C-c C-c") 'vernacular-compile-file)
    (define-key m (kbd "C-c C-z") 'vernacular-switch-repl)
    (define-key m (kbd "C-c C-i C-v") 'vernacular-import-vars)
    (define-key m (kbd "C-c C-i C-f") 'vernacular-import-fns)
    (define-key m (kbd "C-c C-i C-m") 'vernacular-import-module)
    (define-key m (kbd "C-c C-m") 'vernacular-expand-module)
    m))

(define-minor-mode vernacular-mode
  "Minor mode for Vernacular."
  :lighter " Vernacular"
  :keymap vernacular-mode-keymap)

(provide 'vernacular-common)
