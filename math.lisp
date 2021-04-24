(in-package :cl-mu-utils)

;; (defun sma (list)
;;   "Return simple moving average of list of numbers."
;;   (/ (apply #'+ list)
;;      (length list)))

;; (defun ema (list)
;;   "Return exponential moving average of list of numbers."
;;   (let* ((length (length list))
;;          (a      (/ 2 (1+ length))))
;;     (/ (loop
;;           with i = 0
;;           for value in list
;;           sum (prog1
;;                   (* value
;;                         (expt (- 1 a) i))
;;                 (incf i)))
;;        (loop
;;           for i from 0 below length
;;           sum (expt (- 1 a) i)))))

(defun mean (list)
  "Return mean(average, sma) of list of numbers."
  (/ (apply #'+ list)
     (length list)))

(defun variance (list)
  "Return variance of list of numbers(require more than 2 elements)."
  (let ((mean (apply #'mean list)))
    (/ (apply #'+
              (mapcar (lambda (number)
                        (expt (- number mean) 2))
                      list))
       (1- (length list)))))

(defun standard-deviation (list)
  "Return standard deviation of list of numbers."
  (sqrt (variance list)))
