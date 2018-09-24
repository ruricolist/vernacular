(defpackage :vernacular/file-local
  (:documentation "Parse Emacs-style file-local variables.")
  (:use :cl :alexandria :serapeum)
  (:export :file-locals-alist
   :file-emacs-mode))
(in-package vernacular/file-local)

(defconst mode-frob "-*-")

(defun split (delim seq)
  "Split SEQ on DELIM, discarding empty subseqs."
  (split-sequence delim seq :remove-empty-subseqs t))

(def skip-prefixes
  '(
    ;; Shebang.
    "#!"
    ;; Man page.
    "'\\\""
    ;; XML declaration.
    "<?xml")
  "Prefixes that mean we should skip the first line.")

(defun file-locals-alist (file)
  "Return an alist of file-local variables in FILE."
  (with-input-from-file (in file)
    (handler-case
        (let ((line (read-line in)))
          (when (skip-first-line? line)
            (setf line (read-line in)))
          (parse-line line))
      (end-of-file ()
        '()))))

(defun file-emacs-mode (file)
  (values
   (assocdr "mode"
            (file-locals-alist file)
            :test #'equal)))

(defun parse-line (string)
  "Parse an alist of file-local-variables from a string."
  (~>> string
       isolate-substring
       parse-substring))

(defun parse-substring (s)
  "Parse the file-local variables in S."
  (if (hairy? s)
      (parse-hairy s)
      (parse-simple s)))

(defun hairy? (s)
  "Does S contain pairs of variables? (As opposed to a single value,
which is implicitly interpreted as the mode)."
  (some (op (memq _ '(#\: #\;))) s))

(defun parse-hairy (s)
  "Parse a string containing pairs of variables."
  (~>> s
       trim-whitespace
       split-into-pairs
       (mapcar #'split-pair)))

(defun parse-simple (s)
  "Parse a string that contains only the name of the mode."
  (let ((name (trim-whitespace s)))
    (list (cons "mode" name))))

(defun isolate-substring (string)
  "Return the part of STRING between mode frobs."
  (let ((start (search mode-frob string))
        (end (search mode-frob string :from-end t)))
    (and start end
         (subseq string
                 (+ start (length mode-frob))
                 end))))

(defun split-into-pairs (string)
  "Split STRING on semicolons."
  (~>> string
       (split #\; _)
       (mapcar #'trim-whitespace)))

(defun split-pair (string)
  "Split STRING in two on a colon and cons the halves together."
  (destructuring-bind (var value)
      (split #\: string)
    (cons (trim-whitespace var)
          (trim-whitespace value))))

(defun skip-first-line? (line)
  "Assuming LINE is the first line of the file, should we skip it?"
  (some (op (string-prefix-p _ line))
        skip-prefixes))
