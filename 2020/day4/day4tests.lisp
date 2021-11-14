(in-package :2020-day-four)

(checkl:check (:name :part-one-sample)
  (let* ((sample-input (file-string #p"./day4sample.lisp"))
         (passports (parse-passports sample-input)))
    (loop :for passport :in passports
          :count (passport-valid-p passport))))

(checkl:check (:name :part-one)
  (let* ((sample-input (file-string #p"./day4input.lisp"))
         (passports (parse-passports sample-input)))
    (loop :for passport :in passports
          :count (passport-valid-p passport))))
