(in-package :cl-mu-utils)

(let* ((unix-newline (string #\Newline))
       (dos-newline (concatenate 'string
                                 (string #\Return)
                                 unix-newline)))
  (defun dos-string->unix-string (content)
    "Convert Windows text\(CR + LF\) to Unix text\(LF\)."
    (ppcre:regex-replace-all dos-newline content unix-newline))
  (defun unix-string->dos-string (content)
    "Convert Unix text to\(LF\) Windows text\(CR + LF\)."
    (ppcre:regex-replace-all unix-newline content dos-newline)))

(defvar *invalid-character-list*
  (list #\Nul #\Soh #\Stx #\Etx #\Eot #\Enq #\Ack #\Bel #\Backspace #\Vt
        #\Page #\So #\Si #\Dle #\Dc1 #\Dc2 #\Dc3 #\Dc4 #\Nak #\Syn #\Etb
        #\Can #\Em #\Sub #\Esc #\Fs #\Gs #\Rs #\Us #\Rubout)
  "invalid character:
for example:
^H #\\Backspace
^] #\\Gs
^\\ #\\Fs")

(defun contains-invalid-character-p (content)
  (loop for invalid-character in *invalid-character-list*
     when (find invalid-character content)
     do (return-from contains-invalid-character-p t)))

(defun remove-invalid-character (content)
  (dolist (invalid-character *invalid-character-list*)
    (setf content
          (ppcre:regex-replace-all invalid-character
                                   content
                                   "")))
  content)


(defun character-fullwidth->halfwidth (character)
  (check-type character character)
  (case character
    (#\FULLWIDTH_DIGIT_ZERO #\0)
    (#\FULLWIDTH_DIGIT_ONE #\1)
    (#\FULLWIDTH_DIGIT_TWO #\2)
    (#\FULLWIDTH_DIGIT_THREE #\3)
    (#\FULLWIDTH_DIGIT_FOUR #\4)
    (#\FULLWIDTH_DIGIT_FIVE #\5)
    (#\FULLWIDTH_DIGIT_SIX #\6)
    (#\FULLWIDTH_DIGIT_SEVEN #\7)
    (#\FULLWIDTH_DIGIT_EIGHT #\8)
    (#\FULLWIDTH_DIGIT_NINE #\9)
    (#\FULLWIDTH_LATIN_SMALL_LETTER_A #\a)
    (#\FULLWIDTH_LATIN_SMALL_LETTER_B #\b)
    (#\FULLWIDTH_LATIN_SMALL_LETTER_C #\c)
    (#\FULLWIDTH_LATIN_SMALL_LETTER_D #\d)
    (#\FULLWIDTH_LATIN_SMALL_LETTER_E #\e)
    (#\FULLWIDTH_LATIN_SMALL_LETTER_F #\f)
    (#\FULLWIDTH_LATIN_SMALL_LETTER_G #\g)
    (#\FULLWIDTH_LATIN_SMALL_LETTER_H #\h)
    (#\FULLWIDTH_LATIN_SMALL_LETTER_I #\i)
    (#\FULLWIDTH_LATIN_SMALL_LETTER_J #\j)
    (#\FULLWIDTH_LATIN_SMALL_LETTER_K #\k)
    (#\FULLWIDTH_LATIN_SMALL_LETTER_L #\l)
    (#\FULLWIDTH_LATIN_SMALL_LETTER_M #\m)
    (#\FULLWIDTH_LATIN_SMALL_LETTER_N #\n)
    (#\FULLWIDTH_LATIN_SMALL_LETTER_O #\o)
    (#\FULLWIDTH_LATIN_SMALL_LETTER_P #\p)
    (#\FULLWIDTH_LATIN_SMALL_LETTER_Q #\q)
    (#\FULLWIDTH_LATIN_SMALL_LETTER_R #\r)
    (#\FULLWIDTH_LATIN_SMALL_LETTER_S #\s)
    (#\FULLWIDTH_LATIN_SMALL_LETTER_T #\t)
    (#\FULLWIDTH_LATIN_SMALL_LETTER_U #\u)
    (#\FULLWIDTH_LATIN_SMALL_LETTER_V #\v)
    (#\FULLWIDTH_LATIN_SMALL_LETTER_W #\w)
    (#\FULLWIDTH_LATIN_SMALL_LETTER_X #\x)
    (#\FULLWIDTH_LATIN_SMALL_LETTER_Y #\y)
    (#\FULLWIDTH_LATIN_SMALL_LETTER_Z #\z)
    (#\FULLWIDTH_LATIN_CAPITAL_LETTER_A #\A)
    (#\FULLWIDTH_LATIN_CAPITAL_LETTER_B #\B)
    (#\FULLWIDTH_LATIN_CAPITAL_LETTER_C #\C)
    (#\FULLWIDTH_LATIN_CAPITAL_LETTER_D #\D)
    (#\FULLWIDTH_LATIN_CAPITAL_LETTER_E #\E)
    (#\FULLWIDTH_LATIN_CAPITAL_LETTER_F #\F)
    (#\FULLWIDTH_LATIN_CAPITAL_LETTER_G #\G)
    (#\FULLWIDTH_LATIN_CAPITAL_LETTER_H #\H)
    (#\FULLWIDTH_LATIN_CAPITAL_LETTER_I #\I)
    (#\FULLWIDTH_LATIN_CAPITAL_LETTER_J #\J)
    (#\FULLWIDTH_LATIN_CAPITAL_LETTER_K #\K)
    (#\FULLWIDTH_LATIN_CAPITAL_LETTER_L #\L)
    (#\FULLWIDTH_LATIN_CAPITAL_LETTER_M #\M)
    (#\FULLWIDTH_LATIN_CAPITAL_LETTER_N #\N)
    (#\FULLWIDTH_LATIN_CAPITAL_LETTER_O #\O)
    (#\FULLWIDTH_LATIN_CAPITAL_LETTER_P #\P)
    (#\FULLWIDTH_LATIN_CAPITAL_LETTER_Q #\Q)
    (#\FULLWIDTH_LATIN_CAPITAL_LETTER_R #\R)
    (#\FULLWIDTH_LATIN_CAPITAL_LETTER_S #\S)
    (#\FULLWIDTH_LATIN_CAPITAL_LETTER_T #\T)
    (#\FULLWIDTH_LATIN_CAPITAL_LETTER_U #\U)
    (#\FULLWIDTH_LATIN_CAPITAL_LETTER_V #\V)
    (#\FULLWIDTH_LATIN_CAPITAL_LETTER_W #\W)
    (#\FULLWIDTH_LATIN_CAPITAL_LETTER_X #\X)
    (#\FULLWIDTH_LATIN_CAPITAL_LETTER_Y #\Y)
    (#\FULLWIDTH_LATIN_CAPITAL_LETTER_Z #\Z)
    (otherwise character)))

(defun string-fullwidth->halfwidth (string)
  "Replace fullwidth number or letter character to halfwidth character.
Example:
  \(string-fullwidth->halfwidth \"０１２３４５６７８９ａｂｃｄｅｆｇｈｉｊｋｌｍｎｏｐｑｒｓｔｕｖ\
ｗｘｙｚＡＢＣＤＥＦＧＨＩＪＫＬＭＮＯＰＱＲＳＴＵＶＷＸＹＺ\")
  => \"0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ\"
"
  (check-type string string)
  (ppcre:regex-replace-all "[０-９ａ-ｚＡ-Ｚ]"
                           string
                           #'(lambda (target-string
                                      start end
                                      match-start match-end
                                      reg-starts reg-ends)
                               (declare (ignore start end reg-starts reg-ends))
                               (let* ((match-string (subseq target-string
                                                            match-start match-end))
                                      (match-character (coerce match-string 'character)))
                                 (string (character-fullwidth->halfwidth match-character))))))

(defun string-remove-invalid-character (string)
  "Deal with string come from user. Trim invisible useless character, remove invalid character, th\
en replace fullwidth number or letter character to halfwidth character."
  (when (and string
             (stringp string))
    (string-fullwidth->halfwidth
     (remove-invalid-character
      (string-trim '(#\Space #\Newline #\Return) string)))))

(defun chinese-character-p (character)
  (<= #x4E00 (char-code character) #x9FA5))
