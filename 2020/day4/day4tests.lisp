(in-package :2020-day-four)

(checkl:check (:name :part-one-sample)
  (let* ((sample-input (file-string #p"./day4sample.lisp"))
         (passports (parse-passports sample-input)))
    (loop :for passport :in passports
          :count (passport-valid-p passport))))

(checkl:check (:name :part-one)
  (let* ((input (file-string #p"./day4input.lisp"))
         (passports (parse-passports input)))
    (loop :for passport :in passports
          :count (passport-valid-p passport))))

(checkl:check (:name :part-two-valid-samples)
  (mapcar #'p2-passport-valid-p
          (parse-passports (file-string #p"./day4-p2-sample.txt"))))

(checkl:check (:name :part-two-invalid-samples)
  (mapcar #'p2-passport-valid-p
          (parse-passports (file-string #p"./day4-p2-invalid-sample.txt"))))

(checkl:check (:name :part-two)
  (let* ((input (file-string #p"./day4input.lisp"))
         (passports (parse-passports input)))
    (loop :for passport :in passports
          :count (p2-passport-valid-p passport))))
