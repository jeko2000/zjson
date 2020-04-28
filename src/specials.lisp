(in-package #:zjson)

(defvar *optimization-settings*
  '(optimize speed (space 0) (debug 3) (compilation-speed 0))
  "Default optimize settings for declarations.")

(defvar *zjson-input* (make-synonym-stream '*standard-input*)
  "The default stream from which to decode json.")
(declaim (stream *json-input*))

(defvar *zjson-output* (make-synonym-stream '*standard-output*)
  "The default stream to which to encode json.")
(declaim (stream *json-output*))

(defvar *true* :true
  "The symbol representing a json literal true.")
(declaim (symbol true*))

(defvar *false* :false
  "The symbol representing a json literal false.")
(declaim (symbol false*))

(defvar *null* :null
  "The symbol representing a json literal null.")
(declaim (symbol *json-literal-null*))

(defvar *jso-implementation* 'alist-impl
  "The symbol representing the default json-object implementation
class.")
(declaim (symbol *jso-implementation*))

(defvar *indentation* 0
  "The indentation level of the pretty printer. Intended to only be
used internally.")
(declaim (fixnum *indentation*))

(defvar *indentation-char* #\space
  "The indentation character used by the pretty printer. Intended to
only be used internally.")
(declaim (character *indentation-char*))

(defvar *indentation-step* 2
  "The number of indentation characters for each indentation level.
Intended to only be used internally.")
(declaim (fixnum *indentation-step*))
