(defpackage :2020-day-four
  (:use :cl))
(in-package :2020-day-four)

(defun file-string (path)
  (with-open-file (stream path)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      data)))

(defun parse-passports (input-string)
  (let ((grammar '(:alternation
                   (:register #\Newline)
                   (:sequence
                    (:register (:greedy-repetition 1 nil
                                (:char-class :word-char-class #\#)))
                    #\:
                    (:register (:greedy-repetition 1 nil
                                (:char-class :word-char-class #\#)))
                    (:greedy-repetition 0 1 (:char-class #\Newline #\Space)))))
        (passports `(,(make-hash-table :size 8))))
    (ppcre:do-register-groups (newline key val) (grammar input-string nil :sharedp t)
      (if newline
          (push (make-hash-table :size 8) passports)
          (setf (gethash (intern key) (first passports)) val)))
    passports))

(defun passport-valid-p (passport)
  (if (and (gethash '|byr| passport)
           (gethash '|iyr| passport)
           (gethash '|eyr| passport)
           (gethash '|hgt| passport)
           (gethash '|hcl| passport)
           (gethash '|ecl| passport)
           (gethash '|pid| passport))
      t
      nil))
