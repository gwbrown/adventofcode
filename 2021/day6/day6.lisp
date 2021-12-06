(defpackage :2021-day6
  (:use :cl))
(in-package :2021-day6)

(defun read-fishes (in)
  "Reads numbers from a comma-separated line of number supplied as an input stream."
  (loop :for number = (read in nil 'done)
          :then (read in nil 'done)
        :do (read-char in nil nil) ;; discard the comma
        :until (eql 'done number)
        :collecting number :into numbers
        :finally (return numbers)))

(defun update-fish (fish)
  "Updates a single fish for one day. Returns the new state of the fish as the first value,
and whether this fish reproduced this day as the second value."
  (if (= 0 fish)
      (values 6 t)
      (values (1- fish) nil)))

(defun update-fishes (fishes)
  "Destructively modifies `fishes' to update it for the passage of one day"
  (let* ((added-fish '())
         (updated-fishes (mapcan (lambda (fish)
                                 (multiple-value-bind (updated-fish add-fish)
                                     (update-fish fish)
                                   (when add-fish (push 8 added-fish))
                                   (list updated-fish)))
                                 fishes)))
    (nconc added-fish updated-fishes)))

(defun unwrap (maybe-list)
  "If `maybe-list' is a cons, returns `(first maybe-list)', otherwise returns
`maybe-list'."
  (if (consp maybe-list)
      (first maybe-list)
      maybe-list))

;; OLD - doesn't improve things enough
(defun fast-update-fishes (fishes)
  (let* ((added-fish '()))
    (loop :for remaining-fishes :on fishes
          :do (multiple-value-bind (updated-fish add-fish?)
                  (update-fish (unwrap remaining-fishes))
                (when add-fish?
                  (push 8 added-fish))
                (setf (first remaining-fishes) updated-fish)))
    (nconc added-fish fishes)))

(defun simulate-fishes (original-fishes days)
  "Simulates `days' days worth of fish reproduction"
  (let ((fishes original-fishes))
    (dotimes (i days)
      (setf fishes (fast-update-fishes fishes)))
    fishes))

(with-open-file (in #p"./input.txt")
  (let* ((fishes (read-fishes in))
         (final-fishes (time (simulate-fishes fishes 120))))
    (cons (length final-fishes) final-fishes)))
