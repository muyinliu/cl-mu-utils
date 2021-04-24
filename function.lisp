(in-package :cl-mu-utils)

(defun partial (function &rest arguments1)
  (lambda (&rest arguments2)
    (apply function (append arguments1 arguments2))))

(defun memoize (fn)
  (let ((cache (make-hash-table :test #'equal)))
    #'(lambda (&rest args)
        (multiple-value-bind (result exists)
            (gethash args cache)
          (if exists
              result
              (setf (gethash args cache)
                    (apply fn args)))))))

(defun memoize-function (function-name)
  (let ((fn (symbol-function function-name)))
    (setf (symbol-function function-name)
          (let ((cache (make-hash-table :test #'equal)))
            #'(lambda (&rest args)
                (multiple-value-bind 
                      (result exists)
                    (gethash args cache)
                  (if exists
                      result
                      (setf (gethash args cache)
                            (apply fn args)))))))))
