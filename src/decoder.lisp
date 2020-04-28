(in-package #:zjson)

(defun decode (&optional (stream *zjson-input*))
  "Decode and return the next json value from STREAM."
  (declare #.*optimization-settings*)
  (skip-whitespace stream)
  (let ((peek (%peek-char stream)))
    (cond
      ((char= peek +string-delimiter+) (decode-string stream))
      ((char= peek +begin-object-char+) (decode-jso stream))
      ((char= peek +begin-array-char+) (decode-json-array stream))
      ((or (digit-char-p peek) (char= peek +minus-char+)) (decode-number stream))
      ((char= peek #\t) (decode-true stream))
      ((char= peek #\f) (decode-false stream))
      ((char= peek #\n) (decode-null stream))
      (t (signal-decode-error "Don't know how to dispatch on character: ~:c" peek)))))

(defun decode-from-string (string)
  "Decode and return the next json value from STRING."
  (declare #.*optimization-settings*)
  (with-input-from-string (s string)
    (decode s)))

(defun decode-string (stream)
  "Decode a json string from STREAM."
  (declare #.*optimization-settings*)
  (flet ((decode-unicode ()
           ;; Add support for surrogate pairs
           (code-char
            (+ (* 4096 (digit-char-p (read-char stream) 16))
               (* 256 (digit-char-p (read-char stream) 16))
               (* 16 (digit-char-p (read-char stream) 16))
               (digit-char-p (read-char stream) 16)))))
    (with-output-to-string (out)
      (loop
        initially (%read-expected-char-ignoring-whitespace stream +string-delimiter+)
        for next-char = (%read-char stream)
        until (char= next-char +string-delimiter+)
        do (write-char
            (cond ((char= next-char +escape-char+)
                   ;; if we reach the escape character, we dispatch on following char
                   (setf next-char (%read-char stream))
                   (cond
                     ((char= next-char #\n) #\newline)
                     ((char= next-char #\t) #\tab)
                     ((char= next-char #\f) #\formfeed)
                     ((char= next-char #\r) #\return)
                     ((char= next-char #\b) #\backspace)
                     ((char= next-char #\u) (decode-unicode))
                     (t next-char)))
                  (t next-char))
            out)))))

(defun decode-jso (stream)
  "Decode a json object from STREAM."
  (declare #.*optimization-settings*)
  (flet ((decode-pair ()
           (let (key value)
             (setf key (decode-string stream))
             (%read-expected-char-ignoring-whitespace stream #\:)
             (setf value (decode stream))
             (values key value))))
    (loop
      initially (%read-expected-char-ignoring-whitespace stream +begin-object-char+)
      with first = t
      with json = (make-jso)
      for peek = (progn (skip-whitespace stream) (%peek-char stream))
      until (char= peek +end-object-char+)
      do (if first
             (setf first nil)
             (%read-expected-char-ignoring-whitespace stream +value-separator+))
         (multiple-value-bind (key value) (decode-pair)
           (setf (jso-get json key) value))
      finally (%read-expected-char-ignoring-whitespace stream +end-object-char+)
              (return json))))

(defun decode-json-array (stream)
  "Decode a json array from STREAM."
  (declare #.*optimization-settings*)
  (loop
    initially (%read-expected-char-ignoring-whitespace stream +begin-array-char+)
    with first = t
    for peek = (progn (skip-whitespace stream) (%peek-char stream))
    until (char= peek +end-array-char+)
    do (if first
           (setf first nil)
           (%read-expected-char-ignoring-whitespace stream +value-separator+))
    collect (decode stream)
    finally (%read-expected-char-ignoring-whitespace stream +end-array-char+)))

(defun decode-number (stream)
  "Decode a json number from STREAM."
  (declare #.*optimization-settings*)
  (skip-whitespace stream)
  (let* ((value (with-output-to-string (out)
                  (loop for peek = (peek-char nil stream nil nil nil)
                        while (and peek (numericp peek))
                        do (read-char stream nil nil nil)
                           (write-char peek out))))
         (maybe-number (read-from-string value)))
    (if (typep maybe-number 'number)
        maybe-number
        (signal-decode-error "Unable to parse JSON number from ~s" value))))

(defun decode-true (stream)
  "Decode a json literal true from STREAM."
  (declare #.*optimization-settings*)
  (%read-expected-char stream #\t)
  (%read-expected-char stream #\r)
  (%read-expected-char stream #\u)
  (%read-expected-char stream #\e)
  *true*)

(defun decode-false (stream)
  "Decode a json literal false from STREAM."
  (declare #.*optimization-settings*)
  (%read-expected-char stream #\f)
  (%read-expected-char stream #\a)
  (%read-expected-char stream #\l)
  (%read-expected-char stream #\s)
  (%read-expected-char stream #\e)
  *false*)

(defun decode-null (stream)
  "Decode a json literal null from STREAM."
  (declare #.*optimization-settings*)
  (%read-expected-char stream #\n)
  (%read-expected-char stream #\u)
  (%read-expected-char stream #\l)
  (%read-expected-char stream #\l)
  *null*)
