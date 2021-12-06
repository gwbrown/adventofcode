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

(defun run-part-one ()
  (with-open-file (in #p"./input.txt")
    (let* ((fishes (read-fishes in))
           (final-fishes (time (simulate-fishes fishes 80))))
      (cons (length final-fishes) final-fishes))))


;; It has become clear that the old ways are no longer sufficient. We can do better.

(defun group-fishes (fishes)
  "Takes a list of fishes and transforms it into a list of groups, where each group's value
is the number of fish in that group. The head of the list is the fish with a timer of 0,
the second is the fish with a timer of 1, and so on."
  (let ((groups (make-list 9 :initial-element 0)))
    (dolist (fish fishes)
      (incf (first (nthcdr fish groups))))
    groups))

(defun step-groups (groups)
  "Takes a list of groups and destructively processes it, returning a new list of
groups that represents the state of the (lanternfish) world after one day."
  (let* ((fish-that-bred (pop groups)))
    (incf (first (nthcdr 6 groups)) fish-that-bred) ;; The fish that bred today will do so again in 6 days
    (nconc groups (list fish-that-bred)))) ;; The fish that were created today go in the 8th slot

(defun simulate-fish-groups (fishes days)
  "Simulates `days' worth of fish reproduction, using groups"
  (let ((groups (group-fishes fishes)))
    (dotimes (i days)
      (setf groups (step-groups groups)))
    groups))

(defun run-part-two (days)
  (with-open-file (in #p"./input.txt")
    (let* ((fishes (read-fishes in))
           (final-fishes (time (simulate-fish-groups fishes days))))
      (values (apply #'+ final-fishes) final-fishes))))
