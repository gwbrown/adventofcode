(in-package :2020-day-three)

(defun read-map (pathname)
  (with-open-file (in pathname)
    (loop :for line = (read-line in nil 'done)
            :then (read-line in nil 'done)
          :until (eq line 'done)
          :collecting line)))

(checkl:check (:name :part-one-sample)
  (let ((map (read-map #p"./day3sample.txt")))
    (count-tree-hits map 3 1)))

(checkl:check (:name :part-one)
    (let ((map (read-map #p"./day3input.txt")))
      (count-tree-hits map 3 1)))

(checkl:check (:name :part-two-sample)
  (let ((map (read-map #p"./day3sample.txt")))
    (mul-slope-counts map '((1 . 1) (3 . 1) (5 . 1) (7 . 1) (1 . 2)))))

(let ((map (read-map #p"./day3input.txt")))
    (mul-slope-counts map '((1 . 1) (3 . 1) (5 . 1) (7 . 1) (1 . 2))))
