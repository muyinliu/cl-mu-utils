(in-package :cl-user)

(defpackage cl-mu-utils-test
  (:use :cl :prove))

(in-package :cl-mu-utils-test)

(plan nil)

(subtest "-> test"
  (subtest "-> test with simple form"
    (is-expand (mu-utils:-> var1
                            (+ 1))
               (+ var1 1))
    (is-expand (mu-utils:-> var1
                            (+))
               (+ var1)))
  (subtest "-> test with function name only"
    (is-expand (mu-utils:-> var1
                            1-)
               (1- var1)))
  (subtest "-> test with lambda form"
    (is-expand (mu-utils:-> var1
                            (lambda (x) (/ x 5)))
               ((lambda (x) (/ x 5))
                var1)))
  (subtest "-> test with complex form"
    (is-expand (mu-utils:-> var1
                            (+ 1)
                            1-
                            (* 3)
                            (lambda (x) (/ x 5)))
               ((lambda (x) (/ x 5))
                (* (1- (+ var1 1)) 3)))))

(subtest "->> test"
  (subtest "->> test with simple form"
    (is-expand (mu-utils:->> var1
                             (+ 1))
               (+ 1 var1))
    (is-expand (mu-utils:->> var1
                             (+))
               (+ var1)))
  (subtest "->> test with function name only"
    (is-expand (mu-utils:->> var1
                             1+)
               (1+ var1)))
  (subtest "->> test with lambda form"
    (is-expand (mu-utils:->> var1
                             (lambda (x) (/ x 5)))
               ((lambda (x) (/ x 5))
                var1)))
  (subtest "->> test with complex form"
    (is-expand (mu-utils:->> var1
                             (+ 1)
                             1-
                             (* 3)
                             (lambda (x) (/ x 5)))
               ((lambda (x) (/ x 5))
                (* 3 (1- (+ 1 var1)))))))

(subtest "as-> test"
  (subtest "as-> test with simple form"
    (is-expand (mu-utils:as-> var1 x
                              (+ x 1))
               (let* ((x var1))
                 (+ x 1))))
  (subtest "as-> test with complex form"
    (is-expand (mu-utils:as-> var1 x
                              (+ x 1)
                              (1- x)
                              (* 3 x))
               (let* ((x var1)
                      (x (+ x 1))
                      (x (1- x)))
                 (* 3 x)))))

(finalize)
