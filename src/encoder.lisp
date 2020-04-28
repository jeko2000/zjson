(in-package #:zjson)

(defgeneric encode (object &optional stream)
  (:method (object &optional stream)
    (declare (ignore stream))
    (signal-encode-error "There is no applicable method to json encode ~a." object))
  (:documentation "Encode OBJECT to STREAM as json and return nil."))

(defmethod encode :around (object &optional stream)
  (declare (ignore object stream))
  (call-next-method)
  nil)

(defun encode-to-string (object)
  "Return a OBJECT encoded as a json string."
  (declare #.*optimization-settings*)
  (with-output-to-string (s)
    (encode object s)))

(defmethod encode ((integer integer) &optional (stream *zjson-output*))
  (declare #.*optimization-settings*)
  (write integer :stream stream))

(defmethod encode ((number number) &optional (stream *zjson-output*))
  (declare #.*optimization-settings*)
  ;; TODO: Handle corner cases
  (write number :stream stream))

(defmethod encode ((string string) &optional (stream *zjson-output*))
  (declare #.*optimization-settings*)
  (encode-string string stream))

(defun encode-string (string stream)
  "Encode STRING to STREAM as json."
  (declare #.*optimization-settings*)
  (declare (string string) (stream stream))
  (let ((string (coerce string 'simple-string)))
    (loop
      initially (write-char +string-delimiter+ stream)
      for char across string
      do (cond
           ((char= char #\") (write-string "\\\"" stream))
           ((char= char #\\) (write-string "\\\\" stream))
           ((char= char #\newline) (write-string "\\n" stream))
           ((char= char #\return) (write-string "\\r" stream))
           ((char= char #\page) (write-string "\\p" stream))
           ((char= char #\tab) (write-string "\\t" stream))
           ((char= char #\/) (write-string "\\/" stream))
           (t (let ((code (char-code char)))
                (if (or (<= 0 code #x1f) (<= #x7f code #x9f) (<= #x2000 code #x20ff))
                    (format stream "\\u~4,'0x" code)
                    (write-char char stream)))))
      finally (write-char +string-delimiter+ stream))))

(defmethod encode ((literal (eql *true*)) &optional (stream *zjson-output*))
  (declare #.*optimization-settings*)
  (declare (ignore literal))
  (write-string "true" stream))

(defmethod encode ((literal (eql *false*)) &optional (stream *zjson-output*))
  (declare #.*optimization-settings*)
  (declare (ignore literal))
  (write-string "false" stream))

(defmethod encode ((literal (eql *null*)) &optional (stream *zjson-output*))
  (declare #.*optimization-settings*)
  (declare (ignore literal))
  (write-string "null" stream))

(defmethod encode ((objects list) &optional (stream *zjson-output*))
  (declare #.*optimization-settings*)
  (loop
    initially (write-char +begin-array-char+ stream)
    with first = t
    for object in objects
    do (cond (first (setf first nil))
             (t (write-char +value-separator+ stream)))
       (encode object stream)
    finally (write-char +end-array-char+ stream)))

(defmethod encode ((object jso) &optional (stream *zjson-output*))
  (declare #.*optimization-settings*)
  (write-char +begin-object-char+ stream)
  (let ((first t))
    (jso-map (lambda (key value)
               (if first (setf first nil) (write-char +value-separator+ stream))
               (encode-string key stream)
               (write-char +name-separator+ stream)
               (encode value stream))
             object))
  (write-char +end-object-char+ stream))

;; Pretty encoder

(defmacro with-indentation (&body body)
  `(let ((*indentation* (+ *indentation* *indentation-step*)))
     ,@body))

(declaim (inline newline-and-indent))
(defun newline-and-indent (stream)
  (declare #.*optimization-settings*)
  (loop initially (terpri stream)
        repeat *indentation*
        do (write-char *indentation-char* stream)))

(defgeneric encode-prettily (object &optional stream)
  ;; Default to encode generic
  (:method (object &optional (stream *zjson-output*))
    (encode object stream))
  (:documentation "Encode OBJECT prettily to STREAM as json."))

(defun encode-prettily-to-string (object)
  (declare #.*optimization-settings*)
  (with-output-to-string (s)
    (encode-prettily object s)))

(defmethod encode-prettily :around (object &optional stream)
  (declare #.*optimization-settings*)
  (declare (ignore object stream))
  (call-next-method)
  nil)

(defmethod encode-prettily ((objects list) &optional (stream *zjson-output*))
  (declare #.*optimization-settings*)
  (write-char +begin-array-char+ stream)
  (when objects
    (with-indentation
      (loop initially (newline-and-indent stream)
            with first = t
            for object in objects
            do (cond (first (setf first nil))
                     (t (write-char +value-separator+ stream)
                        (newline-and-indent stream)))
               (encode-prettily object stream)))
    (newline-and-indent stream))
  (write-char +end-array-char+ stream))

(defmethod encode-prettily ((object jso) &optional (stream *zjson-output*))
  (declare #.*optimization-settings*)
  (write-char +begin-object-char+ stream)
  (unless (jso-null object)
    (with-indentation
      (newline-and-indent stream)
      (let ((first t))
        (jso-map (lambda (key value)
                   (cond (first (setf first nil))
                         (t (write-char +value-separator+ stream)
                            (newline-and-indent stream)))
                   (encode-string key stream)
                   (write-char +name-separator+ stream)
                   (write-char #\Space stream)
                   (encode-prettily value stream))
                 object)))
    (newline-and-indent stream))
  (write-char +end-object-char+ stream))
