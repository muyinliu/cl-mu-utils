(in-package :cl-mu-utils)

(defun safe-subseq (sequence begin &optional end)
  "Safe version of subseq with available end."
  (let ((length (length sequence)))
    (when (< begin length)
      (if (and end (< end length))
          (subseq sequence begin end)
          (subseq sequence begin)))))

(defun groups (n sequence)
  "Split sequence to at most n groups."
  (let ((hash-table (make-hash-table)))
    (dotimes (i n)
      (setf (gethash i hash-table) nil))
    (loop
       with index = 0
       for element in sequence
       do (let ((i (mod index n)))
            (setf (gethash i hash-table)
                  (append (gethash i hash-table)
                          (list element)))
            (incf index))
       finally (return (rutils:hash-table-vals hash-table)))))

(defun group-by (sequence key
                 &key
                   (test #'equal))
  "Group sequence by result of calling key function of element,
  return type: alist."
  (let ((hash-table (make-hash-table :test test)))
    (loop
      for element in sequence
      do (let* ((value (funcall key element))
                (existing-elements (gethash value hash-table)))
           (if existing-elements
               (setf (gethash value hash-table)
                     (append existing-elements
                             (list element)))
               (setf (gethash value hash-table)
                     (list element))))
      finally (return (rutils:hash-table-to-alist hash-table)))))

(defun vector->hex-string (vector)
  "Convert hash vector to hex string.
  Example:
    (hash-vector->hex-string #(255 60 231 27 41 91 170 146 137 58 197 100 116 195 81 150))
 => \"ff3ce71b295baa92893ac56474c35196\"

  Note: can be replaced with ironclad:byte-array-to-hex-string"
  (format nil "~(~{~2,'0X~}~)"
    (loop for i across vector collect i)))

(defun random-nth (sequence)
  (let ((length (length sequence)))
    (nth (random length) sequence)))

(defun random-nths (sequence count &key length)
  (let ((length (or length (length sequence))))
    (loop for i from 1 to count
       collect (nth (random length) sequence))))

(defun find-duplicates (sequence &key (test #'eq))
  (loop
    with sub-sequence = sequence
    when (member (first sub-sequence)
                 (rest sub-sequence)
                 :test test)
      collect (first sub-sequence)
    do (if (rest sub-sequence)
           (setf sub-sequence
                 (rest sub-sequence))
           (loop-finish))))
