(in-package #:zjson)

(define-condition zjson-error (simple-error)
  ()
  (:documentation "All errors signaled by ZJSON are of this type."))

(define-condition zjson-decode-error (zjson-error)
  ()
  (:documentation "Signaled if ZJSON encounters an error while
decoding."))

(define-condition zjson-decode-eof-error (zjson-error)
  ()
  (:documentation "Signaled if ZJSON encounters end-of-file
unexpectedly."))

(define-condition zjson-decode-unexpected-char (zjson-error)
  ()
  (:documentation "Signaled if ZJSON encounters an unexpected
chracater."))

(define-condition zjson-encode-error (zjson-error)
  ()
  (:documentation "Signaled if ZJSON encounters an error while
encoding a json value."))

(define-condition zjson-pointer-error (zjson-error)
  ()
  (:documentation "Signaled if ZJSON while reading a json pointer."))

(defmacro signal-error (condition format-control &rest format-arguments)
  `(error ,condition
          :format-control ,format-control
          :format-arguments (list ,@format-arguments)))

(defmacro signal-decode-error (format-control &rest format-arguments)
  `(signal-error 'zjson-decode-error ,format-control ,@format-arguments))

(defmacro signal-decode-eof-error (stream)
  `(signal-error 'zjson-decode-eof-error "Unexpected end-of-file on ~s" ,stream))

(defmacro signal-decode-unexpected-char (stream expected-char found-char)
  `(signal-error 'zjson-decode-unexpected-char "Expected ~:c and found ~:c in ~s"
                 ,expected-char ,found-char ,stream))

(defmacro signal-encode-error (format-control &rest format-arguments)
  `(signal-error 'zjson-encode-error ,format-control ,@format-arguments))

(defmacro signal-pointer-error (format-control &rest format-arguments)
  `(signal-error 'zjson-pointer-error ,format-control ,@format-arguments))
