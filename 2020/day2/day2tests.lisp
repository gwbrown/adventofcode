(in-package :2020-day-two)

(checkl:check (:name :sample1) (valid-password-p "1-3 a: abcde"))
(checkl:check (:name :sample2) (valid-password-p "1-3 b: cdefg"))
(checkl:check (:name :sample3) (valid-password-p "2-9 c: ccccccccc"))

(checkl:check (:name :part-one)
  (with-open-file (in #p"./day2input.txt")
    (loop :for line = (read-line in nil 'done)
            :then (read-line in nil 'done)
          :until (eq line 'done)
          :count (valid-password-p line))))

(checkl:check (:name :p2-sample1) (p2-valid-password-p "1-3 a: abcde"))
(checkl:check (:name :p2-sample2) (p2-valid-password-p "1-3 b: cdefg"))
(checkl:check (:name :p2-sample3) (p2-valid-password-p "2-9 c: ccccccccc"))

(checkl:check (:name :part-two)
  (with-open-file (in #p"./day2input.txt")
    (loop :for line = (read-line in nil 'done)
            :then (read-line in nil 'done)
          :until (eq line 'done)
          :count (p2-valid-password-p line))))
