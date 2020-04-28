(defpackage #:zjson/test
  (:use #:cl #:zjson #:fiveam))

(in-package #:zjson/test)

(def-suite zjson-test-suite
  :description "Top-level suite for zjson.")

(defun run-tests! ()
  (run! 'zjson-test-suite))

(in-suite zjson-test-suite)

(defun set-equal? (set1 set2)
  (and
   (subsetp set1 set2 :test #'equal)
   (subsetp set2 set1 :test #'equal)))

(defmacro test-implementation (implementation)
  `(let ((*jso-implementation* ',implementation))
     (let ((jso1 (make-jso "test" t "order" "1017"))
           (jso2 (make-jso "order" "1017" "test" t))
           (jso3 (make-jso "package" "zjson" "test" nil)))
       (is (= 2 (jso-count jso1)))
       (is (= 2 (jso-count jso2)))
       (is (= 2 (jso-count jso3)))
       (is (not (jso-null jso1)))
       (is (not (jso-null jso2)))
       (is (not (jso-null jso3)))
       (is (string= "1017" (jso-get jso1 "order")))
       (is (set-equal? '("test" "order") (jso-keys jso1)))
       (is (set-equal? '("test" "order") (jso-keys jso2)))
       (is (not (set-equal? '("test" "order") (jso-keys jso3))))
       (is (set-equal? '(t "1017") (jso-values jso1)))
       (is (set-equal? '(t "1017") (jso-values jso2)))
       (is (not (set-equal? '(t "1017") (jso-values jso3))))
       (is (string= "default" (jso-get jso2 "key" "default")))
       ;; testing mutating actions
       (setf (jso-get jso1 "test2") "value2")
       (setf (jso-get jso1 "test") nil)
       (jso-remove jso2 "test")
       (jso-clear jso3)
       (is (string= "value2" (jso-get jso1 "test2")))
       (is (null (jso-get jso1 "test")))
       (is (= 1 (jso-count jso2)))
       (is (jso-null jso3))
       ;; encoder
       (is (string= "1" (encode-to-string 1)))
       (is (string= "-1" (encode-to-string -1)))
       (is (string= "\"value\"" (encode-to-string "value")))
       (is (string= "\"\"" (encode-to-string "")))
       (is (string= "true" (encode-to-string :true)))
       (is (string= "false" (encode-to-string :false)))
       (is (string= "null" (encode-to-string :null)))
       (is (string= "[]" (encode-to-string nil)))
       (is (string= "[\"1\",1,true]" (encode-to-string '("1" 1 :true))))
       (is (string= "{}" (encode-to-string (make-jso))))
       (is (string= "[{\"key\":[1,2,3]},true]"
                    (encode-to-string (list (make-jso "key" '(1 2 3)) :true))))

       ;; decoder
       (signals zjson-decode-eof-error (decode-from-string "{"))
       (signals zjson-decode-error (decode-from-string "abc")))))

(test json-implementation-tests
  (test-implementation hash-impl)
  (test-implementation alist-impl))

(test decode-tests
  (flet ((%decode-array (string)
           (with-input-from-string (s string)
             (zjson::decode-json-array s)))
         (%decode-jso (string)
           (with-input-from-string (s string)
             (zjson::decode-jso s))))
    ;; decode json array
    (signals zjson-decode-unexpected-char (%decode-array "42"))
    (signals zjson-decode-eof-error (%decode-array ""))
    (signals zjson-decode-eof-error (%decode-array "["))
    (signals zjson-decode-eof-error (%decode-array "[42"))
    (signals zjson-decode-eof-error (%decode-array "[42,"))
    (is (null (%decode-array "[]")))
    (is (jso= (list 42) (%decode-array "[42]")))
    (is (jso= (list 42 (list "test")) (%decode-array "[42, [\"test\"]]")))
    (is (jso= (list (make-jso "key" "value")) (%decode-array "[{\"key\": \"value\"}]")))
    (is (jso= (list *true* *false* *null*) (%decode-array "[true,false,null]")))))
