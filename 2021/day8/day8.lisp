(defpackage :2021-day8
  (:use :cl))
(in-package :2021-day8)

(defun read-input-line (in)
  (let ((line (read-line in nil nil)))
    (when line
      (with-input-from-string (in line)
        (let* ((signals (read-delimited-list #\| in))
               (digits (loop :repeat 4 :collect (read in))))
          (list signals digits))))))

(defun read-input-lines (in)
  (loop :for line = (read-input-line in)
          :then (read-input-line in)
        :while line
        :collect line))

(defun which-digit (digit)
  (let* ((dgt (symbol-name digit))
         (len (length dgt)))
    (case len
      ((2) 'one)
      ((3) 'seven)
      ((4) 'four)
      ((7) 'eight)
      (t nil))))

(defun solve-part-one ()
  (with-open-file (in #p"./input.txt")
    (let* ((input-lines (read-input-lines in))
           (digits (mapcan #'cadr input-lines)))
      (loop :for digit :in digits
            :when (which-digit digit)
              :count digit))))
