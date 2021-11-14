(defpackage :2020-day-three
  (:use :cl))

(in-package :2020-day-three)

(declaim (optimize (debug 3)))

(defun count-tree-hits (map x-step y-step)
  "Counts the number of trees hit in `map',
starting from the upper left corner and progressing
right `x-step' and down `y-step' blocks each
step. Returns the number of trees hit.'"
  (loop ;;:with y-length = (length map)
        :with x-length = (length (first map))
        ;; :for y-pos :from 0 :to (length map) :by y-step
        :for remaining-y
          :in (nthcdr y-step map) :by (lambda (rest) (nthcdr y-step rest))
        :for x-pos = (mod x-step x-length) :then (mod (+ x-pos x-step) x-length)
        :count (eql #\# (aref remaining-y x-pos))))

(defun mul-slope-counts (map slopes)
  "Multiplies together the hit counts for each of the given slopes,
where `slopes' is of the format `((x-step . y-step))'"
  (let* ((hit-counts (mapcar
                      (lambda (slope)
                        (let ((x-step (first slope))
                              (y-step (rest slope)))
                          (count-tree-hits map x-step y-step)))
                      slopes)))
    (apply #'* hit-counts)))
