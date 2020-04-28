;;;; zjson.asd

(defsystem "zjson"
  :description "A reference Common Lisp JSON [en|de]coding library"
  :author "jeko2000 (Johnny Ruiz)"
  :maintainer "jeko2000 (Johnny Ruiz)"
  :mailto "jeko2000@yandex.com"
  :license  "MIT"
  :version (:read-file-form "version.sexp")
  :encoding :utf-8
  :pathname "src/"
  :serial t
  :components ((:file "package")
               (:file "specials")
               (:file "errors")
               (:file "util")
               (:file "zjson")
               (:file "encoder")
               (:file "decoder"))
  :in-order-to ((test-op (test-op "zjson/test"))))

(defsystem "zjson/test"
  :description "zjson library tests"
  :author "jeko2000 (Johnny Ruiz)"
  :mailto "jeko2000@yandex.com"
  :depends-on ("zjson" "fiveam")
  :pathname "t/"
  :components ((:file "zjson-test"))
  :perform (test-op (o c) (symbol-call :zjson/test '#:run-tests!)))
