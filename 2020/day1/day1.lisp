(defpackage :2020-day-one
  (:use :cl))

(in-package :2020-day-one)

(declaim (ftype (function (fixnum fixnum fixnum) boolean) sums-to?))
(defun sums-to? (x y target-sum)
  (= (+ x y) target-sum))

(declaim (ftype (function (fixnum fixnum fixnum fixnum) boolean) triple-sums-to?))
(defun triple-sums-to? (x y z target-sum)
  (= (+ x y z) target-sum))

(defparameter *sample-input* "1721
979
366
299
675
1456")

(defun read-int-list (in)
  (do ((input-numbers '() (push (read in nil 'done) input-numbers)))
      ((eq 'done (first input-numbers)) (rest input-numbers))))

(declaim (ftype (function (list fixnum) (cons fixnum fixnum)) find-sum-pair))
(defun find-sum-pair (int-list target)
  (loop :for remainder :on int-list
        :nconc (loop :with init = (first remainder)
                     :for comparison :in (rest remainder)
                     :when (sums-to? init comparison target)
                       :collect (cons init comparison)
                         :into target-pairs
                     :finally (return target-pairs))))

(declaim (ftype (function (list fixnum) list) find-sum-triple))
(defun find-sum-triple (int-list target)
  (loop :for first-remainder :on int-list
        :nconc (loop :with outer = (first first-remainder)
                     :for second-remainder :on (rest first-remainder)
                     :nconc (loop :with inner = (first second-remainder)
                                  :for comparison :in (rest second-remainder)
                                  :when (triple-sums-to? inner outer comparison target)
                                    :collect (list inner comparison outer)
                                      :into target-triples
                                  :finally (return target-triples)))))

(defun mul-sum-of-pair (int-list target)
  (let* ((pair (first (find-sum-pair int-list target)))
         (a (car pair))
         (b (cdr pair)))
    (* a b)))

(defun mul-sum-of-triple (int-list target)
  (let ((triple (first (find-sum-triple int-list target))))
    (apply #'* triple)))

(with-open-file (in #p"./day1input.txt" :direction :input)
  (let ((int-list (read-int-list in)))
    (time (mul-sum-of-pair int-list 2020))))

(with-open-file (in #p"./day1input.txt" :direction :input)
  (let ((int-list (read-int-list in)))
    (time (mul-sum-of-triple int-list 2020))))
