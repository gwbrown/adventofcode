(defpackage :2021-day2
  (:use :cl))
(in-package :2021-day2)


(defun read-commands (in)
  (do* ((command nil (read in nil 'done))
        (arg nil (read in nil 'done))
        (command-list '() (push (list command arg) command-list)))
       ((or (eq 'done command) (eq 'done arg)) (nreverse (rest command-list)))))


;; `state' consists of pairs of the form `(HORIZONTAL . VERTICAL)'
(defun exec-command (state command)
  (let ((direction (first command))
        (amount (second command))
        (horizontal (first state))
        (vertical (rest state)))
    (cond
      ((eq 'forward direction) (cons (+ horizontal amount) vertical))
      ((eq 'back direction) (cons (- horizontal amount) vertical))
      ((eq 'down direction) (cons horizontal (+ vertical amount)))
      ((eq 'up direction) (cons horizontal (- vertical amount))))))

(defun exec-commands (commands &optional (state '(0 . 0)))
  (reduce #'exec-command commands :initial-value state))

(with-open-file (in #p"./day2sample.txt")
  (exec-commands (read-commands in)))

(with-open-file (in #p"./day2input.txt")
  (let* ((final-position (exec-commands (read-commands in)))
         (horizontal (first final-position))
         (vertical (rest final-position)))
    (* horizontal vertical)))

;; Part 2

;; `state' is now a list of 3 values: `(HORIZONTAL VERTICAL AIM)'
(defun exec-command-p2 (state command)
  (let ((direction (first command))
        (amount (second command))
        (horizontal (first state))
        (vertical (second state))
        (aim (third state)))
    (cond
      ((eq 'forward direction)
       (list (+ horizontal amount) (+ vertical (* amount aim)) aim))
      ((eq 'down direction)
       (list horizontal vertical (+ aim amount)))
      ((eq 'up direction)
       (list horizontal vertical (- aim amount))))))

(defun exec-commands-p2 (commands &optional (state '(0 0 0)))
  (reduce #'exec-command-p2 commands :initial-value state))

(with-open-file (in #p"./day2sample.txt")
  (exec-commands-p2 (read-commands in)))

(with-open-file (in #p"./day2input.txt")
  (let* ((final-position (exec-commands-p2 (read-commands in)))
         (horizontal (first final-position))
         (vertical (second final-position)))
    (* horizontal vertical)))
