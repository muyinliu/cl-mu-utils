(in-package :cl-mu-utils)

(defmacro with-retry (retry-max &body body)
  "Run code with N retry. If retry N times and still fail, will trigger condition."
  (let ((retry-count-symbol (gensym "retry-count"))
        (retry-max-symbol (gensym "retry-max"))
        (body-with-retry-symbol (gensym "body-with-retry")))
    `(let ((,retry-count-symbol 0)
           (,retry-max-symbol (1- ,retry-max)))
       (labels ((,body-with-retry-symbol ()
                  (restart-case
                      (progn ,@body)
                    (auto-retry ()
                      (incf ,retry-count-symbol)
                      (,body-with-retry-symbol)))))
         (handler-bind ((error #'(lambda (condition)
                                   (declare (ignore condition))
                                   (if (< ,retry-count-symbol ,retry-max-symbol)
                                       (invoke-restart 'auto-retry)
                                       (progn ,@body)))))
           (,body-with-retry-symbol))))))
