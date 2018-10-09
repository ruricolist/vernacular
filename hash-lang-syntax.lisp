(defpackage :vernacular/hash-lang-syntax
  (:use :cl :alexandria :serapeum :vernacular/types)
  (:documentation "Parse the #lang line in a module file.")
  (:import-from :overlord/types :error*)
  (:export
   :file-hash-lang
   :stream-hash-lang
   :skip-hash-lang
   :read-lang-name
   :valid-lang-name?))

(in-package :vernacular/hash-lang-syntax)

(defun file-hash-lang (file &key (external-format :utf-8))
  "Return two values: the name of the lang (as a form) and the position to start reading from."
  (with-input-from-file (stream file :element-type 'character
                                     :external-format external-format)
    (stream-hash-lang stream)))

(-> stream-hash-lang (stream)
    (values (or null string) (integer 0 *)))
(defun stream-hash-lang (stream)
  (skip-comments-and-whitespace stream)
  (flet ((fail () (values nil 0)))
    (if (loop for c across "#lang"
              always (eql c (peek-char nil stream))
              do (read-char stream nil))
        (if-let ((lang-form (read-lang-name stream)))
          (values lang-form (file-position stream))
          (fail))
        (fail))))

(defun skip-hash-lang (stream)
  (assure (integer 0 *)
    (nth-value 1 (stream-hash-lang stream))))

(defun read-lang-name (stream &key errorp)
  (skip-whitespace stream)
  (assure (or string null)
    (let ((token (stream-take-while (complement #'whitespacep) stream)))
      (cond ((valid-lang-name? token)
             token)
            (errorp
             (error* "Invalid language name: ~a" token))
            (t nil)))))

(defun stream-take-while (pred stream)
  (with-output-to-string (out)
    (loop for char = (read-char stream nil)
          for len from 0
          do (cond ((null char) (loop-finish))
                   ((funcall pred char) (write-char char out))
                   (t (unread-char char stream)
                      (loop-finish))))))

(defun lang-char? (char)
  (let ((code (char-code char)))
    (or (<= (char-code #\a) code (char-code #\z))
        (<= (char-code #\A) code (char-code #\Z))
        (<= (char-code #\0) code (char-code #\9))
        ;; NB #\. is not allowed in Racket.
        (in char #\/ #\_ #\- #\+ #\.))))

(defun valid-lang-name? (string)
  (and (stringp string)
       (every #'lang-char? string)))

(defun skip-comments-and-whitespace (stream)
  (loop while (or (skip-whitespace stream) (skip-comment stream))))

(defun skip-whitespace (stream)
  (let ((start (file-position stream)))
    (peek-char t stream)
    (> (file-position stream) start)))

(defun next-char (stream)
  (peek-char nil stream))

(defun skip-comment (stream)
  (let ((char (next-char stream)))
    (cond ((eql char #\;)
           (skip-to-end-of-line stream))
          ((eql char #\#)
           (advance-stream 1 stream)
           (cond ((eql (next-char stream) #\!)
                  (skip-to-end-of-line stream))
                 ;; TODO #|, #;.
                 (t (unread-char #\# stream))))
          (t nil))))

(defun skip-to-end-of-line (stream)
  (loop until (eql (read-char stream) #\Newline)))

(defun advance-stream (n stream)
  (file-position stream (+ n (file-position stream))))
