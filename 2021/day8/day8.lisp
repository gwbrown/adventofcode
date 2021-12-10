(defpackage :2021-day8
  (:use :cl))
(in-package :2021-day8)

(defun normalize-symbol (symbol)
  "Takes a symbol, gets the symbol name, copies it (as sort is destructive),
sorts the copy, and then gets a symbol for that name.
Example: (normalize-symbol 'decba) => 'ABCDE"
  (intern (sort (copy-seq (symbol-name symbol)) #'char<)))

(defun normalize-symbols (symbols)
  (mapcar #'normalize-symbol symbols))

(defun read-input-line (in)
  (let ((line (read-line in nil nil)))
    (when line
      (with-input-from-string (in line)
        (let* ((signals (read-delimited-list #\| in))
               (digits (loop :repeat 4 :collect (read in))))
          (list (normalize-symbols signals) (normalize-symbols digits)))))))

(defun read-input-lines (in)
  (loop :for line = (read-input-line in)
          :then (read-input-line in)
        :while line
        :collect line))

(defun which-digit (digit)
  (let* ((dgt (symbol-name digit))
         (len (length dgt)))
    (case len
      ((2) 'one)
      ((3) 'seven)
      ((4) 'four)
      ((7) 'eight)
      (t nil))))

(defun solve-part-one ()
  (with-open-file (in #p"./input.txt")
    (let* ((input-lines (read-input-lines in))
           (digits (mapcan #'cadr input-lines)))
      (loop :for digit :in digits
            :when (which-digit digit)
              :count digit))))

(defun letterify-digit (digit-symbol)
  (map 'list (lambda (ch) (intern (string ch))) (symbol-name digit-symbol)))

(defun sym-len-eq (symbol target)
  (= target (length (symbol-name symbol))))

(defun digit-with-length (digits length)
  (first (member-if (lambda (digit) (sym-len-eq digit length)) digits)))

(defun find-three (digits one)
  (let ((one-letters (letterify-digit one)))
    (first (member-if (lambda (digit)
                        (let ((digit-letters (letterify-digit digit)))
                          (and (= 5 (length digit-letters))
                               (subsetp one-letters digit-letters))))
                      digits))))

(defun find-five (digits three four)
  (let ((three-letters (letterify-digit three))
        (four-letters (letterify-digit four)))
    (first (member-if (lambda (digit)
                        (let ((digit-letters (letterify-digit digit)))
                          (and (= 5 (length digit-letters))
                               (not (equal three-letters digit-letters))
                               (= 2 (length (set-difference digit-letters four-letters))))))
                      digits))))

(defun find-two (digits three four)
  (let ((three-letters (letterify-digit three))
        (four-letters (letterify-digit four)))
    (first (member-if (lambda (digit)
                        (let ((digit-letters (letterify-digit digit)))
                          ;; (break "digit [~a], diff [~a]" digit (set-difference digit-letters four-letters))
                          (and (= 5 (length digit-letters))
                               (not (equal three-letters digit-letters))
                               (= 3 (length (set-difference digit-letters four-letters))))))
                      digits))))

(defun find-six (digits five nine)
  (let ((five-letters (letterify-digit five)))
    (first (member-if (lambda (digit)
                        (let ((digit-letters (letterify-digit digit)))
                          (and (= 6 (length digit-letters))
                               (not (eq nine digit))
                               (= 0 (length (set-difference five-letters digit-letters))))))
                      digits)
           )))

(defun find-nine (digits three)
  (let* ((three-letters (letterify-digit three)))
    (first (member-if (lambda (digit)
                        (let ((digit-letters (letterify-digit digit)))
                          ;; (if (= 6 (length digit-letters))
                          ;;     (break "digit [~a], diff [~a], ll [~a]" digit (set-difference digit-letters four-letters) lower-left-letter))
                          (and (= 6 (length digit-letters))
                               (= 0 (length (set-difference
                                             three-letters
                                             digit-letters))))))
                      digits))))

(defun find-zero (digits five nine)
  (let ((five-letters (letterify-digit five)))
    (first (member-if (lambda (digit)
                        (let ((digit-letters (letterify-digit digit)))
                          (and (= 6 (length digit-letters))
                               (not (eq nine digit))
                               (= 1 (length (set-difference five-letters digit-letters))))))
                      digits))))

(defun process-signals (entry)
  (destructuring-bind (signals-raw digits-raw) entry
    (declare (ignore digits-raw))
    (let* ((signals signals-raw)
           (one (digit-with-length signals 2))
           (three (find-three signals one))
           (four (digit-with-length signals 4))
           (seven (digit-with-length signals 3))
           (eight (digit-with-length signals 7))
           (five (find-five signals three four))
           (two (find-two signals three four))
           (nine (find-nine signals three))
           (six (find-six signals five nine))
           (zero (find-zero signals five nine))
           (number-map (make-hash-table)))
      ;; (break)
      (setf (gethash one number-map) 1)
      (setf (gethash two number-map) 2)
      (setf (gethash three number-map) 3)
      (setf (gethash four number-map) 4)
      (setf (gethash five number-map) 5)
      (setf (gethash six number-map) 6)
      (setf (gethash seven number-map) 7)
      (setf (gethash eight number-map) 8)
      (setf (gethash nine number-map) 9)
      (setf (gethash zero number-map) 0)
      number-map)))


(defun solve-part-two ()
  (with-open-file (in #p"./input.txt")
    (let* ((input-lines (read-input-lines in))
           (number-maps (mapcar #'process-signals input-lines))
           (displays (mapcar #'cadr input-lines)))
      (loop :for display :in displays
            :for number-map :in number-maps
            ;; :do (break "display: ~a, signals: ~a" (mapcar (lambda (x) (gethash x number-map)) display) display)
            :sum (loop :for digit :in display
                       :for place = 1000 :then (/ place 10)
                       :sum (* place (gethash digit number-map 0)))))))
