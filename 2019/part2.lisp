(ql:quickload '(:alexandria))

(defun read-intcode-tape (intcode-string)
  "Reads an intcode-formatted program (i.e. comma separated integers)"
  (map 'vector #'read-from-string (uiop/utility:split-string intcode-string :separator ",")))

(define-condition end-of-program ()
  ((tape :reader get-tape :initarg :tape :initform (error "must provide tape for end-of-program signal"))
   (pc   :reader get-pc   :initarg :pc :initform (error "must provide pc for end-of-program signal"))))

(defun deref-loc (tape loc)
  "Dereference the location on the tape referenced by a given location"
  (aref tape (aref tape loc)))

(defun set-loc (tape loc value)
  "Set the value on the tape referenced by a given location. Modifies tape."
  (setf (aref tape (aref tape loc)) value)
  tape)

(defun run-opcode-1 (tape pc)
  "Runs opcode 2, as defined by https://adventofcode.com/2019/day/2"
  (let ((pc-value (aref tape pc)))
    (if (/= 1 pc-value)
        (error (format nil "run-opcode-2 called but pc is not 1, is [~a]" pc-value))
        (let ((val-a (deref-loc tape (+ pc 1)))
              (val-b (deref-loc tape (+ pc 2)))
              (dest  (+ pc 3)))
          (set-loc tape dest (+ val-a val-b))))))

(defun run-opcode-2 (tape pc)
  "Runs opcode 2, as defined by https://adventofcode.com/2019/day/2"
  (let ((pc-value (aref tape pc)))
    (if (/= 2 pc-value)
        (error (format nil "run-opcode-2 called but pc is not 2, is [~a]" pc-value))
        (let ((val-a (deref-loc tape (+ pc 1)))
              (val-b (deref-loc tape (+ pc 2)))
              (dest  (+ pc 3)))
          (set-loc tape dest (* val-a val-b))))))

(defun run-intcode-step (tape pc)
  "Runs one step of an intcode program given a tape state (that is, int vector) and a integer
   representing the position of the runner"
  (let ((pc-value (aref tape pc)))
    (cond ((= pc-value 1) (run-opcode-1 tape pc))
          ((= pc-value 2) (run-opcode-2 tape pc))
          ((= pc-value 99) (signal 'end-of-program :tape tape :pc pc))
          (t (error (format nil "invalid opcode [~a] at position [~a]" pc-value pc))))))

(defun run-intcode-program (tape &optional (pc 0))
  (handler-case
      (let ((new-tape (run-intcode-step tape pc)))
        (run-intcode-program new-tape (+ pc 4)))
    (end-of-program (sig)
      (get-tape sig))))

(defun run-intcode (intcode-string)
  (run-intcode-program (read-intcode-tape intcode-string)))

;; test cases

(equalp (run-intcode "1,0,0,0,99") #(2 0 0 0 99))
(equalp (run-intcode "2,3,0,3,99") #(2 3 0 6 99))
(equalp (run-intcode "2,4,4,5,99,0") #(2 4 4 5 99 9801))
(equalp (run-intcode "1,1,1,4,99,5,6,0,99") #(30 1 1 4 2 5 6 0 99))

(defvar *aoc2p1* "1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,1,10,19,1,6,19,23,1,10,23,27,2,27,13,31,1,31,6,35,2,6,35,39,1,39,5,43,1,6,43,47,2,6,47,51,1,51,5,55,2,55,9,59,1,6,59,63,1,9,63,67,1,67,10,71,2,9,71,75,1,6,75,79,1,5,79,83,2,83,10,87,1,87,5,91,1,91,9,95,1,6,95,99,2,99,10,103,1,103,5,107,2,107,6,111,1,111,5,115,1,9,115,119,2,119,10,123,1,6,123,127,2,13,127,131,1,131,6,135,1,135,10,139,1,13,139,143,1,143,13,147,1,5,147,151,1,151,2,155,1,155,5,0,99,2,0,14,0")

;; part 1 solution
(defun run-with-args (program-string arg1 arg2)
  (let ((parsed (read-intcode-tape program-string)))
    (setf (aref parsed 1) arg1)
    (setf (aref parsed 2) arg2)
    (run-intcode-program parsed)))
