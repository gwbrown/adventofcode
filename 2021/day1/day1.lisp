(defpackage :2021-day1
  (:use :cl))
(in-package :2021-day1)


(defun read-int-list (in)
  (do ((input-numbers '() (push (read in nil 'done) input-numbers)))
      ((eq 'done (first input-numbers)) (rest input-numbers))))

(defun count-depth-increases (depths)
  (loop :for remaining :on depths
        :count (let ((first (first remaining))
                     (second (second remaining)))
                 (if (null second)
                     nil
                     (> first second)))))

(with-open-file (in #p"./day1sample.txt")
  (count-depth-increases (read-int-list in)))

(with-open-file (in #p"./day1input.txt")
  (count-depth-increases (read-int-list in)))
