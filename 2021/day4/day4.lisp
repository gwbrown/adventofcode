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

;; It's almost luck that we only need to return the full list
;; of called numbers, rather than including it with each winning
;; board (which you'd need if you wanted to score every board)
(defun play-bingo (boards numbers &optional (win-count 1))
  "Simulates a game of bingo with the given board. Checks numebrs in
the given sequence until `win-count' boards have won (or until all
boards have won if `NIL' is given)."
  (loop ;;:with winning-boards = '()
        :with board-count = (length boards)
        :for boards-in-play = boards
          :then (delete-if
                 (lambda (item) (member item winning-boards))
                 boards-in-play)
        ;; :for i :from 5 :to (1- (length numbers))
        :for remaining-numbers = (nthcdr 4 numbers)
          :then (rest remaining-numbers)
        :for current-numbers = (nreverse (subseq numbers 0 5))
          :then (cons (first remaining-numbers) current-numbers)
        :for wins-this-round = (remove-if-not (lambda (b)
                                                (check-board b current-numbers))
                                              boards-in-play)
          :then (remove-if-not
                 (lambda (b) (check-board b current-numbers))
                 boards-in-play)
        :when wins-this-round
          :append wins-this-round :into winning-boards
        ;; :do (when wins-this-round (break))
        :until (let ((wins (length winning-boards)))
                 (or (eql win-count wins)
                     (eql board-count wins)))
        :finally (return (values winning-boards (nreverse current-numbers)))))

(defun score-board (board numbers-called)
  "Determines the score for a board given the board and the list
of numbers called before it won."
  (loop :for square :from 0 :to (1- (array-total-size board))
        :for square-val = (row-major-aref board square)
          :then (row-major-aref board square)
        :when (not (member square-val numbers-called))
          :sum square-val :into unmarked-total
        :finally (return (* unmarked-total (first (last numbers-called))))))

(with-open-file (in #p"./day4input.txt")
  (let* ((numbers-line (read-line in))
         (numbers (read-numbers-line numbers-line))
         (boards '()))
    (do ((board (read-board in) (read-board in)))
        ((null board) boards)
      (push board boards))
    (multiple-value-bind (winning-boards numbers-called)
        (play-bingo boards numbers nil)
      (cons (score-board (first (last winning-boards)) numbers-called)
            (first (last winning-boards))))))
