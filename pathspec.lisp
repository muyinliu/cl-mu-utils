(in-package :cl-mu-utils)

;;; Path utilities

(defun relative-path (filespec standard-filespec)
  "Return relative path.
Example:
\(relative-path \"/Users/root/log.txt\" \"/Users/root/\")
=> \"log.txt\"

\(relative-path \"/Users/root/log.txt\" \"/Users/\")
=> \"root/log.txt\"
"
  (let* ((namestring (etypecase filespec
                       (pathname (namestring filespec))
                       (string filespec)))
         (standard-namestring (etypecase standard-filespec
                                (pathname (namestring standard-filespec))
                                (string standard-filespec)))
         (index (search standard-namestring namestring)))
    (if (and index (> index -1))
        (let ((begin-index (+ index (length standard-namestring))))
          (subseq namestring begin-index))
        "")))

(defun copy-directory-file (pathname from-directory to-directory
                            &key
                              new-filename
                              (overwrite t))
  (let* ((relative-path (relative-path pathname from-directory))
         (to-pathname (merge-pathnames
                       (if new-filename
                           (format nil "~A~A"
                                   (subseq relative-path
                                           0
                                           (rutils:if-let (end-index
                                                           (search "/" relative-path
                                                                   :from-end t))
                                             (1+ end-index)
                                             0))
                                   new-filename)
                           relative-path)
                       to-directory)))
    (ensure-directories-exist (directory-namestring to-pathname))
    (fad:copy-file pathname to-pathname :overwrite overwrite)))

(defun copy-directory (from-directory to-directory &key (overwrite t))
  "Copy files recursively from directory to directory."
  (fad:walk-directory from-directory
                      #'(lambda (from-pathname)
                          (copy-directory-file from-pathname
                                               from-directory
                                               to-directory
                                               :overwrite overwrite))))

;;; File utilities

(defun file-size (pathspec)
  (with-open-file (stream pathspec :element-type '(unsigned-byte 8))
    (file-length stream)))

(defun file-line-count (pathspec &key (external-format :default))
  (with-open-file (stream pathspec
                          :external-format external-format)
    (loop
       with count = 0
       for line = (read-line stream nil 'eof)
       until (eq line 'eof)
       do (incf count)
       finally (return count))))

(defun file-string-list (filespec &key (external-format :default))
  "Read file into string list"
  (with-open-file (stream filespec
                          :external-format external-format)
    (loop
       for line = (read-line stream nil 'eof)
       until (eq line 'eof)
       collect (remove #\Return line))))

(defun file-string-list-iterate (filespec function
                                 &key
                                   (external-format :default)
                                   (start 0)
                                   (end -1))
  "Iterate file string list, for big files"
  (with-open-file (stream filespec
                          :external-format external-format)
    (loop
       with index = 0
       for line = (read-line stream nil 'eof)
       until (eq line 'eof)
       do (progn
            (when (and (>= index start)
                       (or (eq end -1)
                           (<= index end)))
              (funcall function (remove #\Return line)))
            (incf index)))))

(defun file-string-list-batch-iterate (filespec function
                                       &key
                                         (external-format :default)
                                         (start 0)
                                         (end -1)
                                         (batch-size 1000))
  "Iterate file string list, for big files"
  (with-open-file (stream filespec
                          :external-format external-format)
    (loop
       with index = 0
       with batch-count = 0
       with batch-list = nil
       for line = (read-line stream nil 'eof)
       until (eq line 'eof)
       do (progn
            (when (and (>= index start)
                       (or (eq end -1)
                           (<= index end)))
              (setf batch-list (append batch-list (list (remove #\Return line)))))
            (incf index)
            (incf batch-count)
            (when (> batch-count batch-size)
              (funcall function batch-list)
              (setf batch-count 0
                    batch-list nil)))
       finally (when batch-list
                 (funcall function batch-list)))))

(defun file-string (filespec &key (external-format :default))
  "Read file into single string"
  (with-open-file (stream filespec
                          :external-format external-format)
    (let ((seq (make-array (file-length stream)
                           :element-type 'character
                           :fill-pointer t)))
      (setf (fill-pointer seq)
            (read-sequence seq stream))
      seq)))

(defun file-octets (filespec &key (begin 0) end)
  (with-open-file (stream filespec
                          :element-type '(unsigned-byte 8))
    (let* ((file-length (file-length stream))
           (end (or end file-length))
           (octets (make-array (1+ (- end begin))
                               :element-type '(unsigned-byte 8))))
      (unless (= begin 0)
        (file-position stream begin))
      (read-sequence octets stream)
      octets)))

(defun write-string-to-file (filespec string
                             &key
                               (external-format :default)
                               (if-exists :error))
  (with-open-file (stream filespec
                          :direction :output
                          :external-format external-format
                          :if-exists if-exists)
    (write-string string stream)))

(defun write-octets-to-file (filespec octets
                             &key
                               (if-exists :error)
                               (begin 0)
                               end)
  (with-open-file (stream filespec
                          :direction :output
                          :element-type '(unsigned-byte 8)
                          :if-exists if-exists)
    (write-sequence (if (and (= begin 0)
                             (null end))
                        octets
                        (if end
                            (subseq octets begin end)
                            (subseq octets begin)))
                    stream)))

(defun merge-files (filespec input-filespec-list &key (external-format :default))
  (with-open-file (output-stream filespec
                                 :direction :output
                                 :external-format external-format)
    (dolist (input-filespec input-filespec-list)
      (write-sequence (file-string input-filespec :external-format external-format)
                      output-stream)
      (write-char #\Newline output-stream))))

(defun print-file-tree-inner (parent-pathname &key
                                          (stream t)
                                          (level 0)
                                          (max-level -1)
                                          (print-prefix ""))
  "Print file tree inner component."
  (if (or (equal max-level -1)
          (< level max-level))
      (let* ((pathname-list (fad:list-directory parent-pathname))
             (pathname-list-length (length pathname-list)))
        (dotimes (index pathname-list-length)
          (let* ((pathname (nth index pathname-list))
                 (directory-pathname-p (fad:directory-pathname-p pathname))
                 (file-namestring (if directory-pathname-p
                                      (car (last (pathname-directory pathname)))
                                      (file-namestring pathname))))
            (format stream (concatenate 'string
                                   print-prefix
                                   (if (equal index (1- pathname-list-length))
                                       "└"
                                       "├")
                                   "── ~A~%")
                    file-namestring)
            (when directory-pathname-p
              (print-file-tree-inner pathname
                                     :level (1+ level)
                                     :max-level max-level
                                     :print-prefix (concatenate
                                                    'string
                                                    print-prefix
                                                    (if (equal index
                                                               (1- pathname-list-length))
                                                        " "
                                                        "│")
                                                    "   "))))))))

(defun print-file-tree (parent-pathname &key
                                          (stream t)
                                          (max-level -1))
  (format stream ".~%")
  (print-file-tree-inner parent-pathname :stream stream :max-level max-level))

;; File line filter
(defun file-line-filter (input-pathname output-pathname filter-regex-list
                         &key
                           (delete-match-p t)
                           (input-external-format :utf-8)
                           (output-external-format :utf-8))
  (declare (type list filter-regex-list))
  (let ((match-count 0))
    (with-open-file (input-stream input-pathname
                                  :external-format input-external-format)
      (with-open-file (output-stream output-pathname
                                     :direction :output
                                     :if-exists :supersede
                                     :external-format output-external-format)
        (loop for line = (read-line input-stream nil 'eof)
           until (eq line 'eof)
           do (let ((match-p nil))
                (loop for filter-regex in filter-regex-list
                   when (ppcre:scan filter-regex line)
                   do (progn
                        (setf match-p t)
                        (loop-finish)))
                (when match-p
                  (incf match-count))
                (when (or (and match-p (not delete-match-p))
                          (and (not match-p) delete-match-p))
                  (write-line line output-stream))))))
    match-count))

(defun search-directory (directory regex
                         &key filename-filter-regex
                           identity-p
                           result-handler
                           (external-format :default))
  (let (result)
    (fad:walk-directory
     directory
     (lambda (pathname)
       (let ((filename (file-namestring pathname)))
         (when (or (not filename-filter-regex)
                   (ppcre:all-matches filename-filter-regex filename))
           (let ((content (file-string pathname :external-format external-format)))
             (loop for (start end) in (rutils:group 2 (ppcre:all-matches regex content))
                do (push (subseq content start end) result)))))))
    (funcall (if result-handler result-handler #'identity)
             (if identity-p
                 (remove-duplicates result :test #'equal)
                 result))))

(defun replace-directory (directory regex replacement
                          &key filename-filter-regex (external-format :default))
  (let (result)
    (fad:walk-directory
     directory
     (lambda (pathname)
       (let ((filename (file-namestring pathname)))
         (when (or (not filename-filter-regex)
                   (ppcre:all-matches filename-filter-regex filename))
           (as-> (file-string pathname :external-format external-format) x
                 (ppcre:regex-replace-all regex x replacement)
                 (write-string-to-file pathname x
                                       :external-format external-format
                                       :if-exists :supersede))
           (push pathname result)))))
    result))
                            
