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

(defun parse-int (str)
  (if str
      (parse-integer str :junk-allowed t)
      nil))

(defun validate-height (height)
  (multiple-value-bind
        (match groups) (ppcre:scan-to-strings "(\\d+)(in|cm)" height)
    (when match
      (let ((num (aref groups 0))
            (unit (aref groups 1)))
        (cond
          ((equal "in" unit) (<= 59 (parse-int num) 76))
          ((equal "cm" unit) (<= 150 (parse-int num) 193))
          (t nil))))))

(defun p2-passport-valid-p (passport)
  (let ((byr (gethash '|byr| passport))
        (iyr (gethash '|iyr| passport))
        (eyr (gethash '|eyr| passport))
        (hgt (gethash '|hgt| passport))
        (hcl (gethash '|hcl| passport))
        (ecl (gethash '|ecl| passport))
        (pid (gethash '|pid| passport)))
    (and
     byr (<= 1920 (parse-int byr) 2002)
     iyr (<= 2010 (parse-int iyr) 2020)
     eyr (<= 2020 (parse-int eyr) 2030)
     hgt (validate-height hgt)
     hcl (ppcre:scan "^\#[0-9a-f]{6}$" hcl)
     ecl (ppcre:scan "amb|blu|brn|gry|grn|hzl|oth" ecl)
     pid (ppcre:scan "^[0-9]{9}$" pid)
     t)))
