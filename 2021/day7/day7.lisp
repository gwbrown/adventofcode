(defpackage :2021-day7
  (:use :cl))
(in-package :2021-day7)

(defun read-numbers-line (line)
  "Reads numbers from a `line' supplied as a string."
  (with-input-from-string (in line)
    (loop :for number = (read in nil 'done)
            :then (read in nil 'done)
          :do (read-char in nil nil) ;; discard the comma
          :until (eql 'done number)
          :collecting number :into numbers
          :finally (return numbers))))

(defun fuel-to-move (position target)
  (abs (- target position)))

(defun calculate-fuel-for (positions target)
  (flet ((fuel (position) (fuel-to-move target position)))
    (apply #'+ (mapcar #'fuel positions))))

(defun range (from to)
  (loop :for i :from from :to to
        :collect i))

(defun min-by (by-func list)
  (let ((result (first list)))
    (do* ((remaining list (rest remaining))
          (current-value (funcall by-func (first remaining))
                         (funcall by-func (first remaining)))
          (minimum current-value (if (> minimum current-value)
                                     (progn
                                       (setf result (first remaining))
                                       current-value)
                                     minimum)))
         ((null (rest remaining)) result))))

(defun solve-part-1 () 
  (with-open-file (in #p"./input.txt")
    (let* ((positions (read-numbers-line (read-line in)))
           (possible-targets (range (apply #'min positions) (apply #'max positions)))
           (optimal-target (min-by (lambda (target) (calculate-fuel-for positions target)) possible-targets)))
      ;; (break)
      (values (calculate-fuel-for positions optimal-target) optimal-target))))

(defun fib (n)
  (cond
    ((>= 0 n) 0)
    ((= 1 n)  1)
    (t (+ n (fib (1- n))))))

(defun nl-fuel-to-move (position target)
  "Calculates fuel to move with the nonlinear method"
  (fib (fuel-to-move position target)))

(defun nl-calculate-fuel-for (positions target)
  (flet ((fuel (position) (nl-fuel-to-move target position)))
    (apply #'+ (mapcar #'fuel positions))))

(defun solve-part-2 () 
  (with-open-file (in #p"./input.txt")
    (let* ((positions (read-numbers-line (read-line in)))
           (possible-targets (range (apply #'min positions) (apply #'max positions)))
           ;; (fuels (mapcar (lambda (tgt) (nl-calculate-fuel-for positions tgt)) possible-targets))
           (optimal-target (time (min-by (lambda (target) (nl-calculate-fuel-for positions target)) possible-targets))))
      (break)
      (values (nl-calculate-fuel-for positions optimal-target) optimal-target))))
