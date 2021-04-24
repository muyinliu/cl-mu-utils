(in-package :cl-mu-utils)

(defun string-getbit (string index &key (external-format :utf-8))
  "Similar to SSDB's GETBIT, get bit from a string with index."
  (assert (stringp string))
  (assert (and (integerp index)
               (> index -1)))
  (let ((octets (flex:string-to-octets string :external-format external-format)))
    (if (>= index (* 8 (length octets)))
        0
        (multiple-value-bind (quotient remainder)
            (floor index 8)
          (if (logbitp remainder (elt octets quotient))
              1
              0)))))

(defun string-getbit (string index &key (external-format :utf-8))
  "Similar to Redis's GETBIT, get bit from a string with index."
  (assert (stringp string))
  (assert (and (integerp index)
               (> index -1)))
  (let ((octets (flex:string-to-octets string :external-format external-format)))
    (if (>= index (* 8 (length octets)))
        0
        (multiple-value-bind (quotient remainder)
            (floor index 8)
          (if (logbitp remainder (elt octets quotient))
              1
              0)))))

;; (defun test-getbit ()
;;   "Test getbit of Redis/SSDB.
;; 
;; Redis output:
;; \"0110000101100010011000110110010001100101\"
;; \"0110000101100010011000110110010001100101\"
;;   identical
;; 
;; SSDB's output:
;; \"0110000101100010011000110110010001100101\"
;; \"1000011001000110110001100010011010100110\"
;;   after format:
;; \"01100001 01100010 01100011 01100100 01100101\"
;; \"10000110 01000110 11000110 00100110 10100110\"
;;   the bit order of each character is opposite"
;;   (let* ((key "bitkey")
;;          (value "abcde")
;;          (bit-string (apply #'concatenate 'string
;;                             (loop for character in (coerce value 'list)
;;                                collect (format nil "~8,'0B"
;;                                                (char-code character)))))
;;          (bit-string-length (length bit-string)))
;;     (redis:with-connection-in-pool
;;       (red:set key value)
;;       (let ((bit-string-in-db (apply #'concatenate 'string
;;                                      (loop for i from 0 below bit-string-length
;;                                         collect (write-to-string (red:getbit key i))))))
;;         (values bit-string
;;                 bit-string-in-db)))))
