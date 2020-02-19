#lang eopl

;; Especificación léxica (rhs)
(define especificacion-lexica
  '((espacio (whitespace) skip)
    (comentario ("//" (arbno (not #\newline))) skip)
    (identificador (letter (arbno (or letter digit "-"))) symbol)
    (numero (digit (arbno digit)) number)
    (numero ("-" digit (arbno digit)) number)
    (flotante (digit (arbno digit) "." digit (arbno digit)) number)
    (flotante ("-" digit (arbno digit) "." digit (arbno digit)) number)
    (octal ("0o" (or "1" "2" "3" "4" "5" "6" "7") (arbno (or "1" "2" "3" "4" "5" "6" "7"))) string)
    (hexadecimal ("0x" (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "10" "A" "B" "C" "D" "E" "F")
                       (arbno (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "10" "A" "B" "C" "D" "E" "F"))) string)   
    ))

;; Especificación gramatical (lhs)
(define especificacion-gramatical
  '((programa (expresion) un-programa)
    (expresion (numero) numero-exp)
    (expresion (identificador) identificador-exp)
    (expresion (flotante) flotante-exp)
    (expresion (octal) octal-exp)
    (expresion (hexadecimal) hexadecimal-exp)    
    (expresion ("\"" identificador "\"") string-exp)
    (expresion ("var" "(" (separated-list identificador "=" expresion ",") ")") definicion-exp)
    (expresion ("if" "(" expresion ")" "{" (separated-list expresion ";") "}" "else" "{" (separated-list expresion ";") "}") condicional-exp)
    (expresion ("length" "(" expresion ")") longitud-exp)
    (expresion ("concat" "(" expresion expresion ")") concatenacion-exp)
    (expresion ("function" identificador "(" (separated-list identificador ",") ")" "{" (separated-list expresion ";") "}") procedimiento-exp)
    (expresion ("call" identificador "(" (separated-list expresion ",") ")") invocacion-proc-exp)
    (expresion ("for" "(" expresion ";" expresion ";" expresion ")" "{" (separated-list expresion ";") "}") iteracion-exp)

    (expresion ("function-rec" identificador "(" (separated-list identificador ",") ")" "{" (separated-list expresion ";") "}") procedimiento-rec-exp)
    (expresion ("call-rec" identificador "(" (separated-list expresion ",") ")") invocacion-proc-rec-exp)
    
    (expresion ("(" expresion primitiva-aritmetica expresion ")") primitiva-aritmetica-exp)
    (expresion ("[" expresion primitiva-booleana expresion "]") primitiva-booleana-exp)
    (expresion ("true") verdad-exp)
    (expresion ("false") falso-exp)
    (primitiva-aritmetica ("+") suma-prim)
    (primitiva-aritmetica ("-") resta-prim)
    (primitiva-aritmetica ("*") multiplicacion-prim)
    (primitiva-aritmetica ("/") division-prim)
    (primitiva-aritmetica ("%") modulo-prim)
    (primitiva-aritmetica ("++") incremento-prim)
    (primitiva-aritmetica ("--") decremento-prim)
    (primitiva-booleana ("<") menor-prim)
    (primitiva-booleana (">") mayor-prim)
    (primitiva-booleana ("<=") menor-igual-prim)
    (primitiva-booleana (">=") mayor-igual-prim)
    (primitiva-booleana ("==") igual-prim)
    (primitiva-booleana ("!=") diferente-prim)
    (primitiva-booleana ("&&") conjuncion-prim)
    (primitiva-booleana ("||") disyuncion-prim)
    (primitiva-booleana ("!") negacion-prim)
    ))

;; Creación de los datatypes
(sllgen:make-define-datatypes especificacion-lexica especificacion-gramatical)

;; Muestra de los datatypes creados
(define the-datatypes (lambda () (sllgen:show-define-datatypes especificacion-lexica especificacion-gramatical)))

;; El interpretador
(define interpretador
  (sllgen:make-rep-loop
   "c://"
   (lambda (programa) (evaluar-programa programa))
   (sllgen:make-stream-parser
    especificacion-lexica
    especificacion-gramatical)))

;; Función evalúar programa, que extrae el componente "expresion" de "un-programa"
(define evaluar-programa
  (lambda (pgm)
    (cases programa pgm
      (un-programa (expresion) expresion))))

;; Ejecución del interpretador
(interpretador)

;; Ejemplos
;function funcionX (a, b, c) {if ([s || [f && g]]) {(5+(6+9))} else {"hola"} }
;function funcionY (a, b, c) {var(x=6); if ([s || [f && g]]) {(5+(6+9))} else {"hola"} }
;if ([[(a/2)>0] && [(a/2)==0]]) {var(x=2); "correcto"} else {"malo"; "peor"}
;for (var(i=1); [i < 9]; (i ++ 1)) {var(a=2, b=5); "hola"}
;function-rec funcion-recursiva (x, y, z) {var(o=call-rec funcion-recursiva (1, 2, 3))}
;0x700FDA
;0o74563
