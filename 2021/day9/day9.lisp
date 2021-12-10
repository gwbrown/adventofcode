(defpackage :2021-day9
  (:use :cl))
(in-package :2021-day9)

(defun read-lines (in)
  (loop :for line = (read-line in nil nil)
          :then (read-line in nil nil)
        :while line
        :collect line))

(defun parse-map (lines)
  (let* ((rows (length lines))
         (columns (length (first lines)))
         (map (make-array (list rows columns) :element-type 'fixnum)))
    (loop :for line :in lines
          :for row = 0 :then (1+ row)
          :do (loop :for ch :across line
                    :for column = 0 :then (1+ column)
                    :do (setf (aref map row column) (parse-integer (string ch))))
          :finally (return map))))

(defun low-point-p (map row col)
  (let ((max-row (1- (array-dimension map 0)))
        (max-col (1- (array-dimension map 1)))
        (row-above (1- row))
        (row-below (1+ row))
        (col-left (1- col))
        (col-right (1+ col))
        (point (aref map row col)))
    (cond
      ((and
        (<= 0 row-above max-row)
        (<= (aref map row-above col) point)) nil)
      ((and
        (<= 0 row-below max-row)
        (<= (aref map row-below col) point)) nil)
      ((and
        (<= 0 col-left max-col)
        (<= (aref map row col-left) point)) nil)
      ((and
        (<= 0 col-right max-col)
        (<= (aref map row col-right) point)) nil)
      (t t))))

(defun find-lowest-points (map)
  (loop :for row :from 0 :to (1- (array-dimension map 0))
        :nconc (loop :for col :from 0 :to (1- (array-dimension map 1))
                  :when (low-point-p map row col)
                    :collect (cons row col))))

(defun solve-part-one ()
  (with-open-file (in #p"./input.txt")
    (let* ((lines (read-lines in))
           (map (parse-map lines))
           (lowest-points (find-lowest-points map))
           (lowest-point-vals (mapcar
                               (lambda (pt) (1+ (aref map (first pt) (rest pt))))
                               lowest-points))
           (sum (apply #'+ lowest-point-vals)))
      ;; (break)
      sum)))

