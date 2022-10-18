(in-package :cl-user)

(defpackage cl-mu-utils
  (:use :cl)
  (:nicknames :mu-utils)
  #+:sbcl (:shadow :defconstant)
  #+:sb-package-locks (:lock t)
  (:export
   ;; expand
   #:->
   #:->>
   #:as->
   #:some->
   #:some->>
   #:cond->
   #:cond->>
   ;; pathspec
   #:relative-path
   #:copy-directory-file ;; TODO
   #:copy-directory
   #:file-size
   #:file-line-count
   #:file-string-list
   #:file-string-list-iterate
   #:file-string-list-batch-iterate
   #:file-string
   #:file-octets
   #:write-string-to-file
   #:write-octets-to-file
   #:write-form-to-file
   #:read-form-from-file
   #:merge-files
   #:print-file-tree
   #:file-line-filter
   #:search-directory
   #:replace-directory
   #:move-file
   #:flatten-directory
   ;; sequence
   #:safe-subseq
   #:groups
   #:vector->hex-string
   #:random-nth
   #:random-nths
   #:find-duplicates
   ;; alist
   #:merge-alist-list
   ;; bit
   #:string-getbit
   ;; number
   #:ensure-integer
   #:max-page
   #:randoms
   ;; match
   ;; #:sma
   ;; #:ema
   #:mean
   #:variance
   #:standard-deviation
   ;; character
   #:dos-string->unix-string
   #:unix-string->dos-string
   #:contains-invalid-character-p
   #:remove-invalid-character
   #:character-fullwidth->halfwidth
   #:string-fullwidth->halfwidth
   #:string-remove-invalid-character
   #:chinese-character-p
   ;; string
   #:generate-random-string
   #:price-string-p
   #:digit-string-p
   #:float-string-p
   #:digit-or-float-string-p
   #:empty-p
   #:+email-regex+
   #:email-p
   #:+url-regex+
   #:url-p
   #:contains-chinese-character-p
   #:chinese-string-p
   #:+chinese-regex+
   #:all-matches-as-strings-select-group
   #:all-matches-as-strings-select-groups
   ;; condition
   #:with-retry))


(in-package :cl-mu-utils)

#+sbcl
(defmacro defconstant (name value &optional doc)
  "Make sure VALUE is evaluated only once \(to appease SBCL)."
  `(cl:defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))
