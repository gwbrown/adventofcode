(defpackage :2021-day1-ct
  (:use :coalton :coalton-library))
(in-package :coalton-user)

(coalton-toplevel
  (declare count-depth-increases ((List Integer) -> Integer))
  (define (count-depth-increases depths)
    (progn
      (let doubles = (zip depths (tail depths)))
      (let increases = (map (fn (x) (< (head x) (head (rest x))))))
      (cl:count-if #'identity increases))))
