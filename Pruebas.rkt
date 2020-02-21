#lang eopl

;; Especificación léxica (rhs)
(define especificacion-lexica
  '((espacio (whitespace) skip)
    (comentario ("//" (arbno (not #\newline))) skip)
    (identificador (letter (arbno (or letter digit "-" "_" "?" "¿" "!" "¡"))) symbol)
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
    (expresion ("if" "(" expresion ")" expresion "else" expresion) condicional-exp)
    (expresion ("length" "(" expresion ")") longitud-exp)
    (expresion ("concat" "(" expresion expresion ")") concatenacion-exp)
    (expresion ("function" identificador "(" (separated-list identificador ",") ")" expresion) procedimiento-exp)
    (expresion ("call" identificador "(" (separated-list expresion ",") ")") invocacion-proc-exp)
    (expresion ("for" "(" expresion ";" expresion ";" expresion ")" expresion) iteracion-exp)
    (expresion ("function-rec" identificador "(" (separated-list identificador ",") ")" expresion) procedimiento-rec-exp)
    (expresion ("call-rec" identificador "(" (separated-list expresion ",") ")") invocacion-proc-rec-exp)
    (expresion ("(" expresion primitiva-aritmetica expresion ")") primitiva-aritmetica-exp)
    (expresion ("[" expresion primitiva-booleana expresion "]") primitiva-booleana-exp)
;   (expresion ("|" expresion primitiva-add "|") primitiva-add-exp)
    (expresion ("true") verdad-exp)
    (expresion ("false") falso-exp)
    (primitiva-aritmetica ("+") suma-prim)
    (primitiva-aritmetica ("-") resta-prim)
    (primitiva-aritmetica ("*") multiplicacion-prim)
    (primitiva-aritmetica ("/") division-prim)
    (primitiva-aritmetica ("%") modulo-prim)
    (primitiva-aritmetica ("++") incremento-prim)
    (primitiva-aritmetica ("--") decremento-prim)
;    (primitiva-add ("++") incremento-prim)
;    (primitiva-add ("--") decremento-prim)
    (primitiva-booleana ("<") menor-prim)
    (primitiva-booleana (">") mayor-prim)
    (primitiva-booleana ("<=") menor-igual-prim)
    (primitiva-booleana (">=") mayor-igual-prim)
    (primitiva-booleana ("==") igual-prim)
    (primitiva-booleana ("!=") diferente-prim)
    (primitiva-booleana ("&&") conjuncion-prim)
    (primitiva-booleana ("||") disyuncion-prim)
    (primitiva-booleana ("!") negacion-prim)
    ; Secuenciación
    (expresion ("{" (separated-list expresion ";") "}") secuenciacion-exp)
    ))

;; Creación de los datatypes
(sllgen:make-define-datatypes especificacion-lexica especificacion-gramatical)

;; Muestra de los datatypes creados
(define the-datatypes (lambda () (sllgen:show-define-datatypes especificacion-lexica especificacion-gramatical)))

;; Definición del tipo de dato ambiente
(define-datatype ambiente ambiente?
  (ambiente-vacio)
  (ambiente-extendido
   (id (list-of symbol?))
   (values (list-of value?))
   (amb ambiente?)))

(define value?
  (lambda (x)
    (or(number? x)(procval? x))))
  
;; Definición del ambiente inicial
(define ambiente-inicial
  (lambda ()
    (ambiente-extendido
     '(x y z)
     '(1 2 3)
     (ambiente-vacio))))

;; Definición del tipo de dato clousure
(define-datatype procval procval?
  (closure (list-ids (list-of symbol?))
            (body expresion?)
            (ambiente ambiente?)))

;; El interpretador
(define interpretador
  (sllgen:make-rep-loop
   "c://"
   (lambda (programa) (evaluar-programa programa))
   (sllgen:make-stream-parser
    especificacion-lexica
    especificacion-gramatical)))

;; Función evaluar programa
(define evaluar-expresion
  (lambda (exp ambiente)
    (cases expresion exp
      (numero-exp (numero) numero)
      (flotante-exp (flotante) flotante)
      (octal-exp (octal) octal)
      (hexadecimal-exp (hexadecimal) hexadecimal)
      (identificador-exp (identificador) (apply-env ambiente identificador))
      (string-exp (cadena) cadena)
      (definicion-exp (identificadores valores) (list "var" identificadores valores))
      (condicional-exp (condicion sentencia-verdad sentencia-falsa)
                       (let
                           (
                            (valor-condicion (evaluar-expresion condicion ambiente))
                            )
                         (if (boolean? valor-condicion)
                             (if valor-condicion
                                 (evaluar-expresion sentencia-verdad ambiente)
                                 (evaluar-expresion sentencia-falsa ambiente))
                             (eopl:error "no boolean")))
                           )
      (longitud-exp (cadena) (longitud-cadena (evaluar-expresion cadena ambiente)))
      (concatenacion-exp (cadena1 cadena2) (list "concat" cadena1 cadena2))
      (procedimiento-exp (nombre-funcion parametros cuerpo) (list "function" nombre-funcion parametros cuerpo))
      (invocacion-proc-exp (nombre-funcion argumento) (list "call" nombre-funcion argumento))
      (iteracion-exp (inicial-exp condicion-for incrementador cuerpo) (list "for" inicial-exp condicion-for incrementador cuerpo))
      (procedimiento-rec-exp (nombre-funcion parametros cuerpo) (list "procedimiento" nombre-funcion parametros cuerpo))
      (invocacion-proc-rec-exp (nombre-funcion argumento) (list "llamado" nombre-funcion argumento))
      (primitiva-aritmetica-exp (componente1 operando componente2)
                                (let
                                    (
                                     (op1 (evaluar-expresion componente1 ambiente))
                                     (op2 (evaluar-expresion componente2 ambiente))
                                    )
                                  (evaluar-primitiva operando op1 op2)
                                 )
                                )
      (primitiva-booleana-exp (componente1 operando componente2) (list "booleana" componente1 operando componente2))
      (verdad-exp () #t)
      (falso-exp () #f)
      (secuenciacion-exp (lista-exp) "secuenciacion"))
    ))

;; Función evalúar programa, que extrae el componente "expresion" de "un-programa"
(define evaluar-programa
  (lambda (pgm)
    (cases programa pgm
      (un-programa (expresion) (evaluar-expresion expresion (ambiente-inicial))))))

;; Función que determina la longitud de una cadena
(define longitud-cadena
  (lambda cadena
    (length (string->list (symbol->string (car cadena))))))

;; Función que busca un identificador dentro de un ambiente:
; (Tomado del interpretador_simple del curso)
(define apply-env
  (lambda (env sym)
    (cases ambiente env
      (ambiente-vacio ()
                      (eopl:error 'apply-env "No se encuentra ~s" sym))
      (ambiente-extendido (syms vals env)
                          (let ((pos (list-find-position sym syms)))
                            (if (number? pos)
                                (list-ref vals pos)
                                (apply-env env sym)))))))

;; Función para encontrar la posición de un identificador dentro de un ambiente
; (Tomado del interpretador_simple del curso)
(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                (+ list-index-r 1)
                #f))))))

;; Función que realiza las operaciones aritméticas
(define evaluar-primitiva
  (lambda (op a b)
    (cases primitiva-aritmetica op
      (suma-prim () (+ a b))
      (resta-prim () (- a b))
      (multiplicacion-prim () (* a b))
      (division-prim () (/ a b))
      (modulo-prim () (modulo a b))
      (incremento-prim () (+ a 1))
      (decremento-prim () (- a 1))
      )))  

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
;{var(x=1);if (x) {true} else {false}}
