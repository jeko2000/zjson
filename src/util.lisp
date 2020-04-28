(in-package #:zjson)

(defmacro define-constant (name value &optional doc)
  "Define a global constant NAME with value VALUE. If DOC is set, then
use it as a documentation string."
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

;; json constants
(define-constant +begin-array-char+ #\[)
(define-constant +begin-object-char+ #\{)
(define-constant +end-array-char+ #\])
(define-constant +end-object-char+ #\})
(define-constant +name-separator+ #\:)
(define-constant +value-separator+ #\,)
(define-constant +string-delimiter+ #\")
(define-constant +escape-char+ #\\)
(define-constant +plus-char+ #\+)
(define-constant +minus-char+ #\-)
(define-constant +decimal-point+ #\.)
(define-constant +exponent-upcase+ #\E)
(define-constant +exponent-downcase+ #\e)

(define-constant +numeric-non-digit-char-string+
    (coerce (list +minus-char+ +plus-char+ +decimal-point+
                  +exponent-upcase+ +exponent-downcase+)
            'string)
  "A string of all non-digit (0-9) characters that can be present in
json numbers.")

(declaim (inline numericp))
(defun numericp (char)
  "Return t if char is considered valid in json numbers."
  (declare #.*optimization-settings*)
  (or (digit-char-p char)
      (find char +numeric-non-digit-char-string+ :test #'char=)))

(define-constant +whitespace-char-string+
    (coerce '(#\space #\tab #\linefeed #\return) 'string)
  "A string of all characters considered to be whitespace in
RFC-7159.")

(declaim (inline whitespacep))
(defun whitespacep (char)
  "Return t if CHAR is whitespace."
  (declare #.*optimization-settings*)
  (find char +whitespace-char-string+ :test #'char=))

(defun skip-whitespace (stream)
  "Read contiguous whitespace characters from STREAM."
  (declare #.*optimization-settings*)
  (loop for peek = (peek-char nil stream nil nil nil)
        while (and peek (whitespacep peek))
        do (read-char stream nil nil nil)))

(defun %peek-char (stream)
  "Return the next char in STREAM without reading it. If end-of-file
is reached, then an signal an error."
  (declare #.*optimization-settings*)
  (or (peek-char nil stream nil nil)
      (signal-decode-eof-error stream)))

(defun %read-char (stream)
  "Read STREAM without reading it. If end-of-file is reached, then an
signal an error."
  (declare #.*optimization-settings*)
  (or (read-char stream nil nil nil)
      (signal-decode-eof-error stream)))

(defun %read-expected-char (stream expected-char)
  "Return EXPECTED-CHAR if CHAR= to the next char in STREAM.
Otherwise, signal an error."
  (declare #.*optimization-settings*)
  (let ((char (%read-char stream)))
    (if (char= char expected-char)
        char
        (signal-decode-unexpected-char stream expected-char char))))

(defun %read-expected-char-ignoring-whitespace (stream expected-char)
  "Return EXPECTED-CHAR if CHAR= to the next non-whitespace char in STREAM.
Otherwise, signal an error."
  (declare #.*optimization-settings*)
  (skip-whitespace stream)
  (%read-expected-char stream expected-char))
