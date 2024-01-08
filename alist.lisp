(in-package :cl-mu-utils)

(defun merge-alist-list (alist-list
                         &key
                           (test #'eq)
                           (remove-duplicates-p t)
                           (remove-duplicates-key #'identity)
                           (remove-duplicates-test #'eq)
                           (remove-duplicates-from-end nil))
  (loop
    with result = nil
    for alist in alist-list
    do (loop
         for (key . value-list) in alist
         do (let ((old-value-list (rest (assoc key result
                                               :test test))))
              (if old-value-list
                  ;; key already exists
                  (setf (rest (assoc key result
                                     :test test))
                        (let ((new-value-list (append old-value-list
                                                      value-list)))
                          (if remove-duplicates-p
                              (remove-duplicates new-value-list
                                                 :key remove-duplicates-key
                                                 :test remove-duplicates-test
                                                 :from-end remove-duplicates-from-end)
                              new-value-list)))
                  ;; key NOT exist
                  (setf result
                        (append result
                                (list (cons key value-list)))))))
    finally (return result)))

(defun alist->alist-with-percent-and-accumulate-percent (alist)
  (let ((sum (loop
               for (key . value) in alist
               sum value)))
    (loop
      with accumulate-value = 0
      for (key . value) in alist
      collect (progn
                (incf accumulate-value value)
                (cons key (list value
                                (format nil "~,2F%" (float (/ (* 100 value) sum)))
                                (format nil "~,2F%" (float (/ (* 100 accumulate-value) sum)))))))))

(defun list->item->count-alist (list
                                &key
                                  (test #'equal)
                                  (sort-predicate #'>))
  (loop
    with item->count = (make-hash-table :test test)
    for item in list
    do (if (gethash item item->count)
           (incf (gethash item item->count))
           (setf (gethash item item->count)
                 1))
    finally (return (sort (rutils:hash-table-to-alist item->count)
                          sort-predicate
                          :key #'rest))))
