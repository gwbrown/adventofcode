
(defun valid-password-p (pw)
  (and (= 6 (length pw))
       (string-equal pw (sort (copy-seq pw) #'char<))
       (< (length (remove-duplicates pw)) (length pw))))

(valid-password-p "122345") ;; t
(valid-password-p "111123") ;; t
(valid-password-p "135679") ;; nil
(valid-password-p "111111") ;; t
(valid-password-p "223450") ;; nil
(valid-password-p "123789") ;; nil

(loop for n from 158126 to 624574
      counting (valid-password-p (format nil "~a" n)) into valid-passwords
      finally (return valid-passwords))

(defun duplicate-requirements-p (pw)
  (loop for n from 0 to 9
        collecting (count (digit-char n) pw) into counts
        finally (return (find 2 counts))))

(defun strict-valid-password-p (pw)
  (and (= 6 (length pw))
       (string-equal pw (sort (copy-seq pw) #'char<))
       (duplicate-requirements-p pw)))

(duplicate-requirements-p "112233") ;; T
(duplicate-requirements-p "123444") ;; nil
(duplicate-requirements-p "111122") ;; T

(loop for n from 158126 to 624574
      counting (strict-valid-password-p (format nil "~a" n)) into valid-passwords
      finally (return valid-passwords))
