(in-package :cl-mu-utils)

(defun generate-random-string
    (length &optional
              (character-list '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
                                #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j
                                #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t
                                #\u #\v #\w #\x #\y #\z
                                #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J
                                #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T
                                #\U #\V #\W #\X #\Y #\Z)))
  "Generate random string with seed characters."
  (let ((character-list-length (length character-list))
        (result (make-array length :fill-pointer 0 :adjustable t :element-type 'character)))
    (loop for i from 1 to length
       do (vector-push-extend (nth (random character-list-length)
                                   character-list)
                              result))
    result))


;;; Validation functions

(defun price-string-p (string)
  "Judge whether a string is a price number(float)."
  (when (and string (stringp string))
    (ppcre:scan "^\\d+(\\.\\d+)*$" string)))

(defun digit-string-p (string)
  (when (eq (position #\- string) 0)
    (setf string (subseq string 1)))
  (not (position-if-not #'digit-char-p string)))

(defun float-string-p (string)
  (when (eq (position #\- string) 0)
    (setf string (subseq string 1)))
  (and (eq 1 (count #\. string))
       (not (position-if-not #'digit-char-p
                             (substitute #\0 #\. string)))))

(defun digit-or-float-string-p (string)
  (when (eq (position #\- string) 0)
    (setf string (subseq string 1)))
  (if (find #\. string)
      (float-string-p string)
      (digit-string-p string)))

(defun empty-p (string)
  "Judge whether a stiring is empty or not."
  (or (not string)
      (zerop (length (string-trim " " string)))))

(defconstant +email-regex+
  (ppcre:create-scanner
   "^([a-zA-Z0-9_\\-\.])+@([a-zA-Z0-9_-])+((\.[a-zA-Z0-9_-]{2,3}){1,2})$"))

(defun email-p (string)
  "Judge whether a string is valid E-mail."
  (and (not (empty-p string))
       (ppcre:scan +email-regex+ string)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (interpol:enable-interpol-syntax))

(defconstant +url-regex+
  (ppcre:create-scanner #?"^(https?|HTTPS?|s?ftp|S?FTP|ftps|FTPS):\\/\\/(((([a-zA-Z]|\\d|-|\\.|_|~|[\x{00A0}-\x{D7FF}\x{F900}-\x{FDCF}\x{FDF0}-\x{FFEF}])|(%[\\da-fA-F]{2})|[!\\$&'\\(\\)\\*\\+,;=]|:)*@)?(((\\d|[1-9]\\d|1\\d\\d|2[0-4]\\d|25[0-5])\\.(\\d|[1-9]\\d|1\\d\\d|2[0-4]\\d|25[0-5])\\.(\\d|[1-9]\\d|1\\d\\d|2[0-4]\\d|25[0-5])\\.(\\d|[1-9]\\d|1\\d\\d|2[0-4]\\d|25[0-5]))|((([a-zA-Z]|\\d|[\x{00A0}-\x{D7FF}\x{F900}-\x{FDCF}\x{FDF0}-\x{FFEF}])|(([a-zA-Z]|\\d|[\x{00A0}-\x{D7FF}\x{F900}-\x{FDCF}\x{FDF0}-\x{FFEF}])([a-zA-Z]|\\d|-|\\.|_|~|[\x{00A0}-\x{D7FF}\x{F900}-\x{FDCF}\x{FDF0}-\x{FFEF}])*([a-zA-Z]|\\d|[\x{00A0}-\x{D7FF}\x{F900}-\x{FDCF}\x{FDF0}-\x{FFEF}])))\\.)+(([a-zA-Z]|[\x{00A0}-\x{D7FF}\x{F900}-\x{FDCF}\x{FDF0}-\x{FFEF}])|(([a-zA-Z]|[\x{00A0}-\x{D7FF}\x{F900}-\x{FDCF}\x{FDF0}-\x{FFEF}])([a-zA-Z]|\\d|-|\\.|_|~|[\x{00A0}-\x{D7FF}\x{F900}-\x{FDCF}\x{FDF0}-\x{FFEF}])*([a-zA-Z]|[\x{00A0}-\x{D7FF}\x{F900}-\x{FDCF}\x{FDF0}-\x{FFEF}])))\\.?)(:\\d*)?)(\\/((([a-zA-Z]|\\d|-|\\.|_|~|[\x{00A0}-\x{D7FF}\x{F900}-\x{FDCF}\x{FDF0}-\x{FFEF}])|(%[\\da-fA-F]{2})|[!\\$&'\\(\\)\\*\\+,;=]|:|@)+(\\/(([a-zA-Z]|\\d|-|\\.|_|~|[\x{00A0}-\x{D7FF}\x{F900}-\x{FDCF}\x{FDF0}-\x{FFEF}])|(%[\\da-fA-F]{2})|[!\\$&'\\(\\)\\*\\+,;=]|:|@)*)*)?)?(\\?((([a-zA-Z]|\\d|-|\\.|_|~|[\x{00A0}-\x{D7FF}\x{F900}-\x{FDCF}\x{FDF0}-\x{FFEF}])|(%[\\da-fA-F]{2})|[!\\$&'\\(\\)\\*\\+,;=]|:|@)|[\x{E000}-\x{F8FF}]|\\/|\\?)*)?(#((([a-zA-Z]|\\d|-|\\.|_|~|[\x{00A0}-\x{D7FF}\x{F900}-\x{FDCF}\x{FDF0}-\x{FFEF}])|(%[\\da-fA-F]{2})|[!\\$&'\\(\\)\\*\\+,;=]|:|@)|\\/|\\?)*)?$"))

(defun url-p (string)
  "Judge whether a string is an URL."
  (and (not (empty-p string))
       (ppcre:scan +url-regex+ string)))

(defun internal-url-p (string domain)
  (and (not (empty-p string))
       (ppcre:scan (ppcre:create-scanner (format nil "^(/|https*://([\\w\\d-]+\\.)*~A)"
                                                 (ppcre:quote-meta-chars domain)))
                   string)))

(defun contains-chinese-character-p (string)
  (some #'chinese-character-p
        (coerce string 'list)))

(defun chinese-string-p (string)
  (every #'chinese-character-p
         (coerce string 'list)))

(defconstant +chinese-regex+ (ppcre:create-scanner "[一-龥]"))

(defmacro all-matches-as-strings-select-group (regex target-string group)
  (let ((group-symbols (loop
                          with max-group = group
                          for i from 0 below max-group
                          collect (gensym "GROUP"))))
    `(let ((regex (ppcre:create-scanner ,regex)))
       (mapcar #'(lambda (matched-string)
                   (ppcre:register-groups-bind ,group-symbols
                       (regex
                        matched-string)
                     (declare (ignorable ,@group-symbols))
                     ,(nth (1- group) group-symbols)))
               (ppcre:all-matches-as-strings regex ,target-string)))))

(defmacro all-matches-as-strings-select-groups (regex target-string &rest groups)
  (format t "~S~%" groups)
  (let ((group-symbols (loop
                          with max-group = (apply #'max groups)
                          for i from 0 below max-group
                          collect (gensym "GROUP"))))
    `(let ((regex (ppcre:create-scanner ,regex)))
       (mapcar #'(lambda (matched-string)
                   (ppcre:register-groups-bind ,group-symbols
                       (regex
                        matched-string)
                     (declare (ignorable ,@group-symbols))
                     (list
                      ,@(loop
                           for group in groups
                           collect (nth (1- group) group-symbols)))))
               (ppcre:all-matches-as-strings regex ,target-string)))))

(defun split-string (max-length string)
  "Split string into list of string with max-length"
  (assert (stringp string))
  (assert (and (integerp max-length)
               (> max-length 0)))
  (let ((length (length string)))
    (if (> length 0)
        (loop
          with begin = 0
          with end = (+ begin max-length)
          collect (let ((sub-string (safe-subseq string begin end)))
                    (if sub-string
                        (progn
                          (incf begin max-length)
                          (incf end max-length)
                          sub-string)
                        (loop-finish))))
        '(""))))
