(in-package #:zjson)

(defclass jso ()
  ((store
    :initarg :store
    :documentation "The internal state holding the json object
entries."
    :accessor store)))

(declaim (ftype (function (jso) fixnum) jso-count))
(defgeneric jso-count (json-object)
  (:documentation "Return the number of entries in JSON-OBJECT."))

(defgeneric jso-null (json-object)
  (:documentation "Return t if JSON-OBJECT has zero entries."))

(defgeneric jso-get (json-object key &optional default)
  (:documentation "Find the entry in JSON-OBJECT whose key is KEY and
return its associated value and t as multiple values. Return DEFAULT
and nil if no such entry with KEY exists. Entries can be added
using SETF."))

(defgeneric (setf jso-get) (value json-object key)
  (:documentation "Set the value associated with KEY to VALUE in
JSON-OBJECT."))

(defgeneric jso-keys (json-object)
  (:documentation "Return a list of all keys in JSON-OBJECT."))

(defgeneric jso-values (json-object)
  (:documentation "Return a list all values in JSON-OBJECT."))

(defgeneric jso-remove (json-object key)
  (:documentation "Remove the entry in JSON-OBJECT associated with
KEY. Return t if there was such an entry or nil otherwise."))

(defgeneric jso-clear (json-object)
  (:documentation "Remove all entries from JSON-OBJECT."))

(defgeneric jso-map (function json-object)
  (:documentation "Apply FUNCTION to each entry in JSON-OBJECT and
return nil. FUNCTION should accept the entry's key and value as two
separate parameters."))

(defun as-boolean (form)
  "Returns the value of *LITERAL-TRUE* if FORM is non-nil. Otherwise,
return *LITERAL-FALSE*."
  (declare #.*optimization-settings*)
  (if form *true* *false*))

(defun make-jso (&rest pairs)
  "Return a new json object populated with entries from PAIRS."
  (declare #.*optimization-settings*)
  (let ((json (make-instance *jso-implementation*)))
    (loop for (key value) on pairs by #'cddr
          do (setf (jso-get json key) value))
    json))

(defun jsop (object)
  "Return t when OBJECT is of type jso."
  (declare #.*optimization-settings*)
  (typep object 'jso))

(defun jso= (jso1 jso2)
  (cond ((eq jso1 jso2) t)
        ((stringp jso1) (and (stringp jso2) (string= jso1 jso2)))
        ((numberp jso1) (and (numberp jso2) (= jso1 jso2)))
        ((consp jso1)
         (and (consp jso2)
              (jso= (car jso1) (car jso2))
              (jso= (cdr jso1) (cdr jso2))))
        ((jsop jso1)
         (and (jsop jso2)
              (= (jso-count jso1) (jso-count jso2))
              (block comparison
                (jso-map (lambda (key value1)
                           (multiple-value-bind (value2 value2-p)
                               (jso-get jso2 key)
                             (unless (and value2-p (jso= value1 value2))
                               (return-from comparison nil))))
                         jso1)
                t)))
        (t nil)))

;; IMPLEMENTATIONS

;; hash-table implementation
;; TODO: How should we handle hash-table initargs?
(defclass hash-impl (jso)
  ()
  (:default-initargs
   :store (make-hash-table :test 'equal)))

(defmethod jso-count ((json-object hash-impl))
  (declare #.*optimization-settings*)
  (hash-table-count (store json-object)))

(defmethod jso-null ((json-object hash-impl))
  (declare #.*optimization-settings*)
  (zerop (jso-count json-object)))

(defmethod jso-get ((json-object hash-impl) key &optional default)
  (declare #.*optimization-settings*)
  (gethash key (store json-object) default))

(defmethod (setf jso-get) (value (json-object hash-impl) key)
  (declare #.*optimization-settings*)
  (setf (gethash key (store json-object)) value))

(defmethod jso-keys ((json-object hash-impl))
  (declare #.*optimization-settings*)
  (loop for key being the hash-keys of (store json-object)
        collect key))

(defmethod jso-values ((json-object hash-impl))
  (declare #.*optimization-settings*)
  (loop for value being the hash-values of (store json-object)
        collect value))

(defmethod jso-remove ((json-object hash-impl) key)
  (declare #.*optimization-settings*)
  (remhash key (store json-object)))

(defmethod jso-clear ((json-object hash-impl))
  (declare #.*optimization-settings*)
  (clrhash (store json-object)))

(defmethod jso-map (function (json-object hash-impl))
  (declare #.*optimization-settings*)
  (declare (function function))
  (loop for key being the hash-keys of (store json-object)
          using (hash-value value)
        do (funcall function key value)))

;; alist implementation
(defclass alist-impl (jso)
  ()
  (:default-initargs
   :store '()))

(defmethod jso-count ((json-object alist-impl))
  (declare #.*optimization-settings*)
  (let ((store (store json-object)))
    (declare (list store))
    (length store)))

(defmethod jso-null ((json-object alist-impl))
  (declare #.*optimization-settings*)
  (null (store json-object)))

(defmethod jso-get ((json-object alist-impl) key &optional default)
  (declare #.*optimization-settings*)
  (let ((assoc (assoc key (store json-object) :test #'string=)))
    (if assoc
        (values (cdr assoc) t)
        (values default nil))))

(defmethod (setf jso-get) (value (json-object alist-impl) key)
  (declare #.*optimization-settings*)
  (let ((assoc (assoc key (store json-object) :test #'string=)))
    (if assoc
        (setf (cdr assoc) value)
        (push (cons key value) (store json-object)))
    value))

(defmethod jso-keys ((json-object alist-impl))
  (declare #.*optimization-settings*)
  (mapcar #'car (store json-object)))

(defmethod jso-values ((json-object alist-impl))
  (declare #.*optimization-settings*)
  (mapcar #'cdr (store json-object)))

(defmethod jso-remove ((json-object alist-impl) key)
  (declare #.*optimization-settings*)
  (with-slots (store) json-object
    (declare (list store))
    (setf store (delete key store :test #'string= :key #'car :count 1))))

(defmethod jso-clear ((json-object alist-impl))
  (declare #.*optimization-settings*)
  (setf (store json-object) nil))

(defmethod jso-map (function (json-object alist-impl))
  (declare #.*optimization-settings*)
  (declare (function function))
  (loop for (key . value) in (store json-object)
        do (funcall function key value)))
