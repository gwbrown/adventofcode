(in-package :2020-day-one)


(checkl:check (:name "Given sample input, full stack")
  (mul-sum-of-pair (with-input-from-string (in *sample-input*)
                     (read-int-list in))
                   2020))

(checkl:check (:name "Real input part 2")
  (with-open-file (in #p"./day1input.txt" :direction :input)
    (let ((int-list (read-int-list in)))
      (mul-sum-of-pair int-list 2020))))
