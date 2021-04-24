(defsystem "cl-mu-utils"
  :name "cl-mu-utils"
  :description "Utilities for Common Lisp by muyinliu."
  :version "0.0.2"
  :author "Muyinliu Xing <muyinliu@gmail.com>"
  :license "MIT"
  :in-order-to ((test-op (test-op "cl-mu-utils-test")))
  :depends-on ("rutils"
               "cl-fad"
               "cl-ppcre"
               "cl-interpol"
               "flexi-streams")
  :components ((:file "packages")
               (:file "expand" :depends-on ("packages"))
               (:file "pathspec" :depends-on ("packages" "expand"))
               (:file "sequence" :depends-on ("packages"))
               (:file "number" :depends-on ("packages"))
               (:file "math" :depends-on ("packages"))
               (:file "character" :depends-on ("packages"))
               (:file "string" :depends-on ("packages"))
               (:file "condition" :depends-on ("packages"))))
