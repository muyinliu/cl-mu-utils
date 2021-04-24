(in-package :cl-mu-utils)

(defun ensure-integer (value &optional (optional 0))
  "Make sure return an integer."
  (ctypecase value
    (integer value)
    (string (parse-integer value :junk-allowed t))
    (t optional)))

(defun max-page (count max-item-each-page &optional max-page)
  "Generate max page number."
  (check-type count integer)
  (let ((result 0))
    (when (> count 0)
      (multiple-value-bind (consult remainder)
          (floor count max-item-each-page)
        (setf result
              (if (> remainder 0)
                  (1+ consult)
                  consult)))
      (when (and max-page
                 (numberp max-page)
                 (> max-page 0)
                 (> result max-page))
        (setf result max-page)))
    result))

(defun randoms (n count)
  "Return random number list."
  (if (<= n count)
      (loop for i from 0 below n
         collect i)
      (let ((*random-state* (make-random-state t)))
        (loop
           with result-count = 0
           with result = nil
           while (< result-count count)
           do (let ((new (random n)))
                (unless (find new result)
                  (push new result)
                  (incf result-count)))
           finally (return result)))))
