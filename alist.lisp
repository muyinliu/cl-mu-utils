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
