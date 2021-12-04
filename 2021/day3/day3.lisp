(defpackage :2021-day3
  (:use :cl))
(in-package :2021-day3)

(declaim (optimize (debug 3) (speed 1)))

(defun read-diagnostic-lines (in)
  (do ((input-lines '() (push (read-line in nil 'done) input-lines)))
      ((eq 'done (first input-lines)) (nreverse (rest input-lines)))))

(defun calc-rates (lines)
  "Calculates the gamma and epsilon rates from the given diagnostic
lines. Returned in the format '(GAMMA EPSILON)"
  (let* ((line-count 0) ;; Could use (length lines) but that's another traversal
         (line-length (length (first lines)))
         (counts (make-array line-length)))
    (dolist (line lines)
      (incf line-count)
      (dotimes (i line-length)
        (when (eql #\1 (aref line i))
          (incf (aref counts i)))))
    (let ((majority-threshold (/ line-count 2))
          (gamma 0)
          (epsilon 0))
      (dotimes (i line-length)
        (let ((bytespec (byte 1 (- (1- line-length) i))))
          (if (< majority-threshold (aref counts i))
              (progn
                (setf (ldb bytespec gamma) 1)
                (setf (ldb bytespec epsilon) 0))
              (progn
                (setf (ldb bytespec gamma) 0)
                (setf (ldb bytespec epsilon) 1)))))
      (list gamma epsilon))))

(defun substring (length source-string &optional (offset 0))
  "Creates a substring of the given source string which is displaced into
the original string, meaning there's no copying of data. Similar to a
Rust slice."
  (make-array length
              :element-type 'character
              :displaced-to source-string
              :displaced-index-offset offset))

(defun resolve-rating (type index)
  "Resolves a single rating ('OXYGEN or 'CO2) using the appropriate
rules for that rating type from the given prefix index. The prefix
index is a hash table, with:
Keys of type: (OR 'OXYGEN 'CO2)
Values of type: (CONS FIXNUM (LIST STRING)))

Where fixnum in the values is the number of values, and the list
is the values themselves (the count is included so we don't have
to traverse the list to find the length)"
  (loop :with type-test = (cond
                            ((eql 'oxygen type) #'<)
                            ((eql 'co2 type) #'>)
                            (t (error "invalid type ~s" type)))
        :with default-value = (cond
                                ((eql 'oxygen type) "1")
                                ((eql 'co2 type) "0")
                                (t (error "invalid type ~s" type)))
        :with prefix = ""
        :until (eql 1 (first (gethash prefix index '(0 . nil))))
        :finally (return (first (rest (gethash prefix index))))
        :do
           (let* ((next-zero (concatenate 'string prefix "0"))
                  (zero-results (gethash next-zero index '(0 . nil)))
                  (zero-candidates (first zero-results))
                  (next-one  (concatenate 'string prefix "1"))
                  (one-results (gethash next-one index '(0 . nil)))
                  (one-candidates (first one-results)))
             ;; (break)
             (cond
               ((eql zero-candidates one-candidates)
                (setf prefix (concatenate 'string prefix default-value)))
               ((funcall type-test zero-candidates one-candidates)
                (setf prefix (concatenate 'string prefix "1")))
               (t (setf prefix (concatenate 'string prefix "0")))))))

(defun calc-ratings (lines)
  "Calculates the oxygen generator ranking and the CO2 scrubber rating
and retuns them in the form '(OXY-RATING CO2-RATING)"
  (let* ((line-count 0)
         (line-length (length (first lines)))
         (index (make-hash-table :test #'equal)))
    ;; Generate the counts an prefix index
    (dolist (line lines)
      (incf line-count)
      (dotimes (i line-length)
        (push line (gethash (substring (1+ i) line) index))))
    (maphash (lambda (prefix vals)
               (setf (gethash prefix index) (cons (length vals) vals)))
             index)
    ;; (break)
    ;; Now loop from 0 to LINE-LENGTH, checking each bit as we go
    (cons (resolve-rating 'oxygen index)
          (resolve-rating 'co2 index))))

;; Run these in the REPL

(with-open-file (in #p"./day3input.txt")
  (let ((rates (calc-rates (read-diagnostic-lines in))))
    (* (first rates) (second rates))))

(with-open-file (in #p"./day3input.txt")
  (let* ((lines (read-diagnostic-lines in))
         (ratings (calc-ratings lines))
         (oxygen-rating (let ((*read-base* 2)) (read-from-string (first ratings))))
         (co2-rating (let ((*read-base* 2)) (read-from-string (rest ratings)))))
    (* oxygen-rating co2-rating)))
