#lang eopl

;(string->list "abcd")

;;(reverse (string->list "abcd"))

; (+(string->number (substring "12333" 1 3))(string->number (substring "12333" 1 3)))
; (make-string 1 #\A)
; (string->number(make-string 1(car (string->list "1bcd")))) convierte cadena de carateres en lista

;(cons '(1 2 5) (list(string->number(make-string 1(car (string->list "1bcd")))))) llenar lista

(define octal-list
  (lambda (num)
    (cond
      [(equal? (substring num 0 2) "0o")(append '('0o) (reverse(string->list(substring num 2 (length(string->list num)))))) ])))
     