(defpackage :2021-day5
  (:use :cl))
(in-package :2021-day5)

(defun read-line-descriptor (in)
  ;; Doesn't use logical let* properties but must run in order
  (let* ((x1 (read in nil nil)) 
         (drop (read-char in nil nil)) ;; First comma
         (y1 (read in nil nil))
         (drop (dotimes (i 3) (read-char in nil nil))) ;; [space]->
         (x2 (read in nil nil))
         (drop (read-char in nil nil)) ;; Second comma
         (y2 (read in nil nil)))
    (declare (ignore drop))
    (if (and x1 y1 x2 y2)
        `((,x1 . ,y1) (,x2 . ,y2))
        nil)))

(defun read-line-descriptors (file)
  (with-open-file (in file)
    (loop :for line = (read-line-descriptor in)
            :then (read-line-descriptor in)
          :while line
          :collecting line)))

(defun apply-y-only-line (x y1 y2 map)
  (let ((y-min (min y1 y2))
        (y-max (max y1 y2)))
    (loop :for y :from y-min :to y-max
          :do (incf (aref map y x))))
  map)

(defun apply-x-only-line (y x1 x2 map)
  (let ((x-min (min x1 x2))
        (x-max (max x1 x2)))
    (loop :for x :from x-min :to x-max
          :do (incf (aref map y x))))
  map)

(defun apply-line (line-descriptor map)
  (destructuring-bind ((x1 . y1) (x2 . y2)) line-descriptor
    (let ((x-diff (- x2 x1))
          (y-diff (- y2 y1))
          (diagonals '()))
      (cond
        ((= 0 x-diff) (apply-y-only-line x1 y1 y2 map))
        ((= 0 y-diff) (apply-x-only-line y1 x1 x2 map))
        (t (push line-descriptor diagonals)))))
  map)

(defun solve-part-1 ()
  (let ((lines (read-line-descriptors #p"./input.txt"))
        (map (make-array '(1000 1000) :initial-element 0 :element-type 'fixnum)))
    (loop :for line :in lines
          :do (apply-line line map))
    (loop :for i :from 0 :to (1- (array-total-size map))
          :count (< 1 (row-major-aref map i)))))
