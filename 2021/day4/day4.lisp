(defpackage :2021-day4
  (:use :cl))
(in-package :2021-day4)

(defun read-board (in)
  (let ((board (make-array '(5 5) :element-type 'fixnum)))
    (dotimes (i 5)
      (dotimes (j 5)
        (let ((square (read in nil nil)))
          ;; (break)
          (when (null square) (return-from read-board nil))
          (setf (aref board i j) square))))
    board))

(defun read-numbers-line (line)
  "Reads numbers from a `line' supplied as a string."
  (with-input-from-string (in line)
    (loop :for number = (read in nil 'done)
            :then (read in nil 'done)
          :collecting number :into numbers
          :do (read-char in nil nil) ;; discard the comma
          :until (eql 'done number)
          :finally (return numbers))))

(defun check-row (board row-idx numbers)
  (loop :for i :from 0 :to 4
        :always (member (aref board row-idx i) numbers)))

(defun check-col (board col-idx numbers)
  (loop :for i :from 0 :to 4
        :always (member (aref board i col-idx) numbers)))

(defun check-board (board numbers)
  "Checks if a board has won given a list of numbers that have been called"
  (loop :for i :from 0 :to 4
        :thereis (or (check-col board i numbers)
                    (check-row board i numbers))))

(defun play-bingo (boards numbers)
  (loop :for i :from 5 :to (1- (length numbers))
        :for current-numbers = (subseq numbers 0 i)
            :then (subseq numbers 0 i)
        :for winning-boards = (remove-if-not (lambda (b)
                                                 (check-board b current-numbers))
                                               boards)
            :then (remove-if-not (lambda (b) (check-board b current-numbers)) boards)
        ;; :do (break)
        :until winning-boards
        :finally (return (values winning-boards current-numbers))))

(with-open-file (in #p"./day4input.txt")
  (let* ((numbers-line (read-line in))
         (numbers (read-numbers-line numbers-line))
         (boards '()))
    (do ((board (read-board in) (read-board in)))
        ((null board) boards)
      (push board boards))
    (multiple-value-bind (winning-boards numbers-called)
        (play-bingo boards numbers)
      (loop :with board = (first winning-boards)
            :for square :from 0 :to (1- (array-total-size board))
            :for square-val = (row-major-aref board square)
              :then (row-major-aref board square)
            :when (not (member square-val numbers-called))
              :sum square-val :into unmarked-total
            :finally (return (* unmarked-total (first (last numbers-called))))))))
