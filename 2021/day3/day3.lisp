(defpackage :2021-day3
  (:use :cl))
(in-package :2021-day3)

(declaim (optimize (debug 3) (speed 1)))

(defun read-diagnostic-lines (in)
  (do ((input-lines '() (push (read-line in nil 'done) input-lines)))
      ((eq 'done (first input-lines)) (nreverse (rest input-lines)))))

(defun calc-rates (lines)
  "Calculates the gamma and epsilon rates from the given diagnostic
lines. Returned in the format '(GAMMA EPSILON)"
  (let* ((line-count 0) ;; Could use (length lines) but that's another traversal
         (line-length (length (first lines)))
         (counts (make-array line-length)))
    (dolist (line lines)
      (incf line-count)
      (dotimes (i line-length)
        (when (eql #\1 (aref line i))
          (incf (aref counts i)))))
    (let ((majority-threshold (/ line-count 2))
          (gamma 0)
          (epsilon 0))
      (dotimes (i line-length)
        (break "counts: [~a], position: [~a], count: [~a], threshold: [~a], g: [~a], e: [~a]"
               counts i (aref counts i) majority-threshold gamma epsilon)
        (let ((bytespec (byte 1 (- (1- line-length) i))))
          (if (< majority-threshold (aref counts i))
              (progn
                (setf (ldb bytespec gamma) 1)
                (setf (ldb bytespec epsilon) 0))
              (progn
                (setf (ldb bytespec gamma) 0)
                (setf (ldb bytespec epsilon) 1)))))
      (list gamma epsilon))))

(with-open-file (in #p"./day3input.txt")
  (let ((rates (calc-rates (read-diagnostic-lines in))))
    (* (first rates) (second rates))))
