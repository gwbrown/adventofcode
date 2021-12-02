(defpackage :2021-day1
  (:use :cl))
(in-package :2021-day1)


(defun read-int-list (in)
  (nreverse (do ((input-numbers '() (push (read in nil 'done) input-numbers)))
                ((eq 'done (first input-numbers)) (rest input-numbers)))))

(defun count-depth-increases (depths)
  (loop :for remaining :on depths
        :count (let ((first (first remaining))
                     (second (second remaining)))
                 (if (null second)
                     nil
                     (< first second)))))

(defun count-depth-increases* (depths)
  (let* ((doubles (mapcar #'list depths (nthcdr 1 depths)))
         (increases (mapcar (lambda (x) (< (first x) (second x))) doubles)))
    (count-if #'identity increases)))

(defun sum-triples (depths)
  (let* ((triples (mapcar #'list depths (nthcdr 1 depths) (nthcdr 2 depths)))
         (sums (mapcar (lambda (triple) (apply #'+ triple)) triples)))
    sums))

(with-open-file (in #p"./day1sample.txt")
  (count-depth-increases* (read-int-list in)))

(with-open-file (in #p"./day1input.txt")
  (count-depth-increases* (read-int-list in)))

(with-open-file (in #p"./day1input.txt")
  (count-depth-increases (sum-triples (read-int-list in))))
