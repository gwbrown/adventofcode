(defpackage :2020-day-two
  (:use :cl))

(in-package :2020-day-two)

(defun parse-password-line (line)
  (destructuring-bind
      (lo-bound hi-bound char junk password)
      (cl-ppcre:split "[ :-]" line)
    (declare (ignore junk))
    (list
     (read-from-string lo-bound)
     (read-from-string hi-bound)
     char
     password)))

(defun valid-password-p (password-line)
  (destructuring-bind
      (lo-bound hi-bound char password)
      (parse-password-line password-line)
    (let* ((matches (cl-ppcre:all-matches (format nil "~a" char) password))
           (num-of-matches (/ (list-length matches) 2)))
      (<= lo-bound num-of-matches hi-bound))))

(defun xor (bool-a bool-b)
  (and
   (or bool-a bool-b)
   (or
    (not bool-a)
    (not bool-b))))

(defun p2-valid-password-p (password-line)
  (destructuring-bind
      (first-pos second-pos char password)
      (parse-password-line password-line)
    (let* ((first-idx (1- first-pos))
           (second-idx (1- second-pos))
           (target-char (aref char 0))
           (pw-length (length password)))
      (if (or (<= pw-length first-idx)
              (<= pw-length second-idx))
          nil ;; index out of bounds!
          (xor (eql
                target-char
                (aref password first-idx))
               (eql
                target-char
                (aref password second-idx)))))))

