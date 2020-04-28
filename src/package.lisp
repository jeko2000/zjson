;;;; package.lisp

(defpackage #:zjson
  (:use #:cl)
  (:export
   #:*json-input*
   #:*json-output*
   #:*true*
   #:*false*
   #:*null*
   #:*jso-implementation*
   #:zjson-error
   #:zjson-decode-error
   #:zjson-decode-unexpected-char
   #:zjson-decode-eof-error
   #:zjson-encode-error
   #:jso
   #:jso-count
   #:jso-null
   #:jso-get
   #:jso-keys
   #:jso-values
   #:jso-remove
   #:jso-clear
   #:jso-map
   #:as-boolean
   #:make-jso
   #:hash-impl
   #:alist-impl
   #:encode
   #:encode-to-string
   #:encode-prettily
   #:encode-prettily-to-string
   #:decode
   #:decode-from-string
   #:decode-string
   #:decode-jso
   #:decode-json-array
   #:decode-number
   #:decode-true
   #:decode-false
   #:decode-null
   #:jsop
   #:jso=))
