(defsystem "cl-mu-utils-test"
  :name "cl-mu-utils-test"
  :description "test case for cl-mu-utils"
  :author "Muyinliu Xing <muyinliu@gmail.com>"
  :depends-on ("cl-mu-utils" "prove")
  :defsystem-depends-on ("prove-asdf")
  :components ((:module "test"
                        :serial t
                        :components ((:file "cl-mu-utils-test"))))
  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
