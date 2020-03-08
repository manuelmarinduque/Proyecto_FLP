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
    (expresion ("var" "(" (separated-list type-exp identificador "=" expresion ",") ")") definicion-exp)
    (expresion ("if" "(" expresion ")" expresion "else" expresion) condicional-exp)
    (expresion ("function" identificador "(" (separated-list type-exp identificador ",") ")" expresion) procedimiento-exp)
    (expresion ("call" identificador "(" (separated-list expresion ",") ")") invocacion-proc-exp)
    ;    (expresion ("for" "(" expresion ";" expresion ";" expresion ")" expresion) iteracion-exp)
    (expresion ("function-rec" identificador "(" (separated-list type-exp identificador ",") ")" expresion) procedimiento-rec-exp)
    (expresion ("call-rec" identificador "(" (separated-list expresion ",") ")") invocacion-proc-rec-exp)
    (expresion ("(" expresion primitiva expresion ")") primitiva-exp)
    (expresion ("[" expresion primitiva2 "]") primitiva2-exp)
    (expresion (primitiva3 "(" expresion ")") longitud-exp)
    (expresion (primitiva4 "(" expresion expresion ")") concatenacion-exp)
    (expresion (primitiva5 expresion) negacion-exp)
    (expresion ("true") verdad-exp)
    (expresion ("false") falso-exp)
    (primitiva ("+") suma-prim)
    (primitiva ("-") resta-prim)
    (primitiva ("*") multiplicacion-prim)
    (primitiva ("/") division-prim)
    (primitiva ("%") modulo-prim)
    (primitiva ("<") menor-prim)
    (primitiva (">") mayor-prim)
    (primitiva ("<=") menor-igual-prim)
    (primitiva (">=") mayor-igual-prim)
    (primitiva ("==") igual-prim)
    (primitiva ("!=") diferente-prim)
    (primitiva ("&&") conjuncion-prim) ;and
    (primitiva ("||") disyuncion-prim) ;or
    (primitiva2 ("++") incremento-prim)
    (primitiva2 ("--") decremento-prim)
    (primitiva3 ("length") longitud-prim)
    (primitiva4 ("concat") concatenacion-prim)
    (primitiva5 ("!") negacion-prim)
    (expresion ("{" expresion (arbno ";" expresion) "}") secuenciacion-exp) ; Secuenciación
    ;    (expresion ("val" expresion "=" expresion) asignacion-exp)
    (expresion ("struct" identificador "{" (separated-list type-exp expresion ";") "}") estructura-exp)
    ;    (expresion ("access" identificador "[" numero "]") acceso-exp)
    (type-exp ("int") int-type-exp)
    (type-exp ("float") float-type-exp)
    (type-exp ("hex") hexadecimal-type-exp)
    (type-exp ("oct") octal-type-exp)
    (type-exp ("string") string-type-exp)
    (type-exp ("bool") bool-type-exp)
    (type-exp ("proc" "(" (separated-list type-exp "*") "->" type-exp ")") proc-type-exp)
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
   (amb ambiente?))
  (ambiente-recursivo-extendido (nombre-procedimiento (list-of symbol?))
                                (parametros (list-of (list-of symbol?)))
                                (cuerpo (list-of expresion?))
                                (ambinte ambiente?)))

(define value?
  (lambda (x)
    (or (number? x)(procval? x)(symbol? x)(string? x)(list? x))))
  
;; Definición del ambiente inicial
(define ambiente-inicial
  (lambda ()
    (ambiente-extendido
     '(x y z)
     (list 1 2 3)
     (ambiente-vacio))))

;; Definición del tipo de dato clousure
(define-datatype procval procval?
  (clousure (list-ids (list-of symbol?))
            (body expresion?)
            (ambiente ambiente?)))

;; El interpretador
(define interpretador
  (sllgen:make-rep-loop
   "c:// "
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
      (hexadecimal-exp (hexadecimal) hexadecimal)
      (octal-exp (octal) octal)
      (string-exp (cadena) cadena)
      (verdad-exp () 'true)
      (falso-exp () 'false)
      (identificador-exp (identificador) (apply-env ambiente identificador))
      (definicion-exp (listatipos identificadores valores) 
        (letrec
            (
             (listavalores (map (lambda (x) (evaluar-expresion x ambiente)) valores))             
             )
          (ambiente-extendido identificadores listavalores ambiente)
          )
        )
      (negacion-exp (operando boolean) (if (equal? (evaluar-expresion boolean ambiente) 'true) 'false 'true))
      (condicional-exp (condicion sentencia-verdad sentencia-falsa)
                       (let
                           (
                            (valor-condicion (evaluar-expresion condicion ambiente))
                            )
                         (if (or (equal? valor-condicion 'true) (equal? valor-condicion 'false))
                             (if (equal? valor-condicion 'true)
                                 (evaluar-expresion sentencia-verdad ambiente)
                                 (evaluar-expresion sentencia-falsa ambiente))
                             (eopl:error "No boolean")))
                       )
      (longitud-exp (operando cadena) (longitud-cadena (evaluar-expresion cadena ambiente)))
      (concatenacion-exp (operando cadena1 cadena2) (concatenacion (evaluar-expresion cadena1 ambiente)
                                                                   (evaluar-expresion cadena2 ambiente)))
      (procedimiento-exp (nombrefuncion listatipos parametros cuerpo)
                         (ambiente-extendido (list nombrefuncion)
                                             (list (clousure parametros cuerpo ambiente))
                                             ambiente))
      (invocacion-proc-exp (nombrefuncion argumentos)
                           (let
                               (
                                (funcion (apply-env ambiente nombrefuncion))
                                (listaargumentos (map (lambda(x) (evaluar-expresion x ambiente)) argumentos))
                                )
                             (if (procval? funcion)
                                 (cases procval funcion
                                   (clousure (listaidentificadores cuerpo ambientepadre)
                                             (if (= (length listaargumentos) (length listaidentificadores))
                                                 (evaluar-expresion cuerpo (ambiente-extendido listaidentificadores listaargumentos ambientepadre))
                                                 (eopl:error "El número de argumentos enviados no corresponden con los recibidos por la función ")
                                                 )
                                             ))
                                 (eopl:error 'invocacion-proc-exp "No existe la funcion ~s" nombrefuncion))
                             )
                           )
      ;      (iteracion-exp (inicial-exp condicion-for incrementador cuerpo)
      ;                     (letrec
      ;                         (
      ;                          (variable (evaluar-expresion2 inicial-exp ambiente))
      ;                          (valor (evaluar-expresion inicial-exp ambiente))
      ;                          (condicion (evaluar-expresion condicion-for ambiente))
      ;                          (nuevo-valor (evaluar-expresion incrementador ambiente))
      ;                          (cuerpo-evaluado (evaluar-expresion cuerpo ambiente))
      ;                          (lista-resultados (if(equal? condicion 'true)
      ;                                               (list cuerpo-evaluado (evaluar-expresion
      ;                                                                      (iteracion-exp inicial-exp condicion-for incrementador cuerpo)
      ;                                                                      (ambiente-extendido (list variable) (list nuevo-valor) ambiente)))
      ;                                               '()))
      ;                          )
      ;                       lista-resultados
      ;                       )
      ;                     )
      (procedimiento-rec-exp (nombre-funcion listatipos parametros cuerpo)
                             (ambiente-extendido-recursivo (list nombre-funcion) (list parametros) (list cuerpo) ambiente)
                             )      
      (invocacion-proc-rec-exp (nombre-funcion argumentos)
                               (let
                                   (
                                    (procedimiento (apply-env ambiente nombre-funcion))
                                    (argumentos-evaluados (map (lambda (x) (evaluar-expresion x ambiente)) argumentos))
                                    )
                                 (if (procval? procedimiento)
                                     (apply-procedure procedimiento argumentos-evaluados)
                                     (eopl:error 'eval-expression "No se encontro el procedimiento recursivo ~s" procedimiento))
                                 )
                               )
      (primitiva-exp (componente1 operando componente2)
                     (let
                         (
                          (op1 (evaluar-expresion componente1 ambiente))
                          (op2 (evaluar-expresion componente2 ambiente))
                          )
                       (evaluar-primitiva operando op1 op2 ambiente)
                       )
                     )
      (primitiva2-exp (componente operando)
                      (let
                          (
                           (op (evaluar-expresion componente ambiente))
                           )
                        (evaluar-primitiva2 operando op)
                        ))
      (secuenciacion-exp (expresion lista-exp) (secuenciacion expresion lista-exp ambiente))
      ;      (asignacion-exp (identificador nuevo-valor) "asignacion")
      (estructura-exp (nombreestructura listatipos lista-exp) (ambiente-extendido (list nombreestructura)
                                                                       (list lista-exp)
                                                                       ambiente))
      ;      (acceso-exp (nombreestructura posicion) "acceso estructura")
      )))

;; Función que realiza la secuenciación
(define secuenciacion
  (lambda (exp exps env)
    (let
        (
         (acc (evaluar-expresion exp env))
         )
      (if (ambiente? acc)
          (if (null? exps)
              acc
              (secuenciacion (car exps) (cdr exps) acc))
          (if (null? exps)
              acc
              (secuenciacion (car exps) (cdr exps) env)))
      )
    ))

;; Función evalúar programa, que extrae el componente "expresion" de "un-programa"
(define evaluar-programa
  (lambda (pgm)
    (cases programa pgm
      (un-programa (expresion) (evaluar-expresion expresion (ambiente-inicial))))))

;; Función que determina la longitud de una cadena
(define longitud-cadena
  (lambda (cadena)
    (length (string->list (symbol->string cadena)))))

;; Función que concatena dos cadenas
(define concatenacion
  (lambda (cadena1 cadena2)
    (string-append (symbol->string cadena1) (symbol->string cadena2))))

;;Funcion hexa-octal
(define octal-list
  (lambda (num)
    (cond
      [(equal? (substring num 0 2) "0o") (append '("0o")(map (lambda (x) (string->number(make-string 1 x )))(reverse(string->list(substring num 2 (length(string->list num))))))) ]
      [(equal? (substring num 0 2) "0x") (append '("0x")(map (lambda (x) (if (eqv? x #\A) 10
                                                                             (if (eqv? x #\B) 11
                                                                                 (if   (equal? x #\C) 12
                                                                                       (if   (equal? x #\D) 13
                                                                                             (if   (equal? x #\E) 14
                                                                                                   (if   (equal? x #\F) 15(string->number(make-string 1 x )))))))))(reverse(string->list(substring num 2 (length(string->list num)))))))])))

;; Función que hace la operacion de conversion
(define list_index-aux
  (lambda(num acc res base )
    (cond
      [(null? num)res]
      [else (list_index-aux (cdr num)(+ 1 acc) (+ (*(expt base acc)(car num)) res) base)])))

(define list_index
  (lambda(num msg)
    (cond
      [(eqv? msg "0o" ) (list msg (list_index-aux num 0 0 8))]
      [(eqv? msg "0x" ) (list msg (list_index-aux num 0 0 16))]
      [else (eopl:error "Numero no valido")])))

;; Función generalizada
(define conversion
  (lambda (num base msg)
    (string-append
     msg
     (number->string (quotient num base) base)
     (number->string (remainder num base) base))))

(define conversion-aux
  (lambda (lis-num)
    (cond
      [(equal? (car lis-num) "0o")(conversion (cadr lis-num) 8 "0o")]
      [(equal? (car lis-num) "0x")(conversion (cadr lis-num) 16 "0x")])))

;; Función que 
(define hace_todo
  (lambda (num)
    (list_index (cdr (octal-list num))  (car (octal-list num)) )))

;;; Funcion para dejar una lista de un solo nivel
;(define planar
;  (lambda (lista)
;    (cond
;      [(null? (car lista)) '()]
;      [(not(list? (car lista)))(append (car lista)(planar (cdr lista)))]
;      [else(append (planar (car lista))(planar (cadr lista)))]
;      )))

;; Creación de un ambiente extendido para funciones recursivas
(define ambiente-extendido-recursivo
  (lambda (nombre-procedimiento parametros cuerpo ambiente-padre)
    (ambiente-recursivo-extendido
     nombre-procedimiento parametros cuerpo ambiente-padre)))

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
                                (apply-env env sym))))
      (ambiente-recursivo-extendido (nombre-procedimiento parametros cuerpo ambiente-padre)
                                    (let
                                        (
                                         (pos (list-find-position sym nombre-procedimiento))
                                         )
                                      (if (number? pos)
                                          (clousure (list-ref parametros pos)
                                                    (list-ref cuerpo pos)
                                                    env)
                                          (apply-env ambiente-padre sym)))
                                    ))))

;; Función que evalúa el cuerpo de un procedimiento en el ambiente extendido correspondiente
; (Tomado del interpretador_recursivo del curso)
(define apply-procedure
  (lambda (proc args)
    (cases procval proc
      (clousure (ids body env)
                (evaluar-expresion body (ambiente-extendido ids args env))))))

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

;; Función que realiza las operaciones aritméticas y booleanas
(define evaluar-primitiva
  (lambda (op a b env)
    (let
        (
         (a (if (string? a) (hace_todo a) a))
         (b (if (string? b) (hace_todo b) b))
         )
      (cases primitiva op
        (suma-prim () (if (and (list? a) (list? b))
                          (conversion-aux(list (car a ) (+(cadr a) (cadr b))))
                          (+ a b)))
        (resta-prim () (if (and (list? a) (list? b))
                           (conversion-aux(list (car a ) (-(cadr a) (cadr b))))
                           (- a b)))
        (multiplicacion-prim () (if (and (list? a) (list? b))
                                    (conversion-aux(list (car a ) (*(cadr a) (cadr b))))
                                    (* a b)))
        (division-prim () (if (and (list? a) (list? b))
                              (conversion-aux(list (car a ) (*(cadr a) (cadr b))))
                              (/ a b)))
        (modulo-prim () (modulo a b))
        (menor-prim ()  (if (and (list? a) (list? b))
                            (evaluar-primitiva op (cadr a) (cadr b) env)
                            (if (< a b) (evaluar-expresion (verdad-exp) env) (evaluar-expresion (falso-exp) env))))
        (mayor-prim () (if (and (list? a) (list? b))
                           (evaluar-primitiva op (cadr a) (cadr b) env)
                           (if (> a b) (evaluar-expresion (verdad-exp) env) (evaluar-expresion (falso-exp) env))))
        (menor-igual-prim () (if (and (list? a) (list? b))
                                 (evaluar-primitiva op (cadr a) (cadr b) env)
                                 (if (<= a b) (evaluar-expresion (verdad-exp) env) (evaluar-expresion (falso-exp) env))))
        (mayor-igual-prim () (if (and (list? a) (list? b))
                                 (evaluar-primitiva op (cadr a) (cadr b) env)
                                 (if (>= a b) (evaluar-expresion (verdad-exp) env) (evaluar-expresion (falso-exp) env))))
        (igual-prim () (if (and (list? a) (list? b))
                           (evaluar-primitiva op (cadr a) (cadr b) env)
                           (if (equal? a b) (evaluar-expresion (verdad-exp) env) (evaluar-expresion (falso-exp) env))))
        (diferente-prim () (if (and (list? a) (list? b))
                               (evaluar-primitiva op (cadr a) (cadr b) env)(if (not (equal? a b)) (evaluar-expresion (verdad-exp) env) (evaluar-expresion (falso-exp) env)))) 
        (conjuncion-prim () (if (and (equal? a 'true) (equal? b 'true)) (evaluar-expresion (verdad-exp) env) (evaluar-expresion (falso-exp) env)))
        (disyuncion-prim () (if (or (equal? a 'true) (equal? b 'true)) (evaluar-expresion (verdad-exp) env) (evaluar-expresion (falso-exp) env)))
        ))))

; Función que realizar el incremento y decremento en 1
(define evaluar-primitiva2
  (lambda (op a)
    (let
        (
         (a (if (string? a)
                (hace_todo a)
                a))
         )
      (cases primitiva2 op
        (incremento-prim () (if (list? a)
                                (conversion-aux(list (car a)(+ (cadr a) 1))) 
                                (+ a 1)))        
        (decremento-prim () (if (list? a)
                                (conversion-aux(list (car a)(- (cadr a) 1)))
                                (- a 1)))
        ))))

;; Definición de tipos                   -------------------------------------------------------------------------------
; (Tomado del interpretador_chequeo del curso)

;; Definición del tipo de dato tipos
(define-datatype type type?
  (atomic-type (name symbol?))
  (proc-type (arg-types (list-of type?))
             (result-type type?)))

;; Definición del ambiente de tipos
(define-datatype ambiente-tipos ambiente-tipos?
  (empty-tenv)
  (extended-tenv (name symbol?)
                 (syms (list-of symbol?))
                 (vals (list-of type?))
                 (tenv ambiente-tipos?)))

;; El Interpretador junto con el analizador de tipos
(define interpretador-tipos
  (sllgen:make-rep-loop
   "c:// "
   (lambda (programa) (aux-interpretador programa)) 
   (sllgen:make-stream-parser
    especificacion-lexica
    especificacion-gramatical)))

(define aux-interpretador
  (lambda (x)
    (if (type? (evaluar-tipo-programa x)) (evaluar-programa  x) 'error)))

;; Función que chequea el tipo de un programa teniendo en cuenta un ambiente dado (se inicializa dentro del programa)
(define evaluar-tipo-programa
  (lambda (pgm)
    (cases programa pgm
      (un-programa (exp) (evaluar-tipo-expresion exp (empty-tenv))))))

;; Función que chequea el tipo de la expresión en el ambiente de entrada
(define evaluar-tipo-expresion
  (lambda (exp tenv)
    (cases expresion exp
      (numero-exp (numero) int-type)
      (flotante-exp (flotante) float-type)
      (hexadecimal-exp (hexadecimal) hexadecimal-type)
      (octal-exp (octal) octal-type)
      (string-exp (cadena) string-type)
      (verdad-exp () bool-type)
      (falso-exp () bool-type)
      (identificador-exp (identificador) int-type)
      (definicion-exp (listatipos identificadores valores) int-type)
      (condicional-exp (test-exp true-exp false-exp)
                       (let ((test-type (evaluar-tipo-expresion test-exp tenv))
                             (false-type (evaluar-tipo-expresion false-exp tenv))
                             (true-type (evaluar-tipo-expresion true-exp tenv)))
                         (check-equal-type! test-type bool-type test-exp)
                         (check-equal-type! true-type false-type exp)
                         true-type))
      (secuenciacion-exp (expresion lista-exp) (secuenciacion-tipos (append (list expresion) lista-exp) tenv))
      (primitiva-exp (componente1 operando componente2)
                     (let (
                           (componente1-eval (evaluar-tipo-expresion componente1 tenv))
                           (componente2-eval (evaluar-tipo-expresion componente2 tenv))
                           )
                       (if (and (equal? componente1-eval int-type) (equal? componente2-eval int-type))
                           (type-of-application
                            (type-of-primitive-int operando)
                            (types-of-expressions
                             (list componente1 componente2) tenv)
                            operando (list componente1 componente2) exp)
                           (if (and (equal? componente1-eval float-type) (equal? componente2-eval float-type))
                               (type-of-application
                                (type-of-primitive-float operando)
                                (types-of-expressions
                                 (list componente1 componente2) tenv)
                                operando (list componente1 componente2) exp)
                               (if (and (equal? componente1-eval bool-type) (equal? componente2-eval bool-type))
                                   (type-of-application
                                    (type-of-primitive-int operando)
                                    (types-of-expressions
                                     (list componente1 componente2) tenv)
                                    operando (list componente1 componente2) exp)
                                   (if (and (equal? componente1-eval hexadecimal-type) (equal? componente2-eval hexadecimal-type))
                                       (type-of-application
                                        (type-of-primitive-hexadecimal operando)
                                        (types-of-expressions
                                         (list componente1 componente2) tenv)
                                        operando (list componente1 componente2) exp)
                                       (if (and (equal? componente1-eval octal-type) (equal? componente2-eval octal-type))
                                           (type-of-application
                                            (type-of-primitive-octal operando)
                                            (types-of-expressions
                                             (list componente1 componente2) tenv)
                                            operando (list componente1 componente2) exp)
                                           (check-equal-type! componente1-eval componente2-eval exp)
                                           )))))))
      (primitiva2-exp (componente operando)
                      (type-of-application
                       (type-of-primitive2 operando)
                       (types-of-expressions
                        (list componente) tenv)
                       operando (list componente) exp))
      (longitud-exp (operando cadena)
                    (type-of-application
                     (type-of-primitive3 operando)
                     (types-of-expressions
                      (list cadena) tenv)
                     operando (list cadena) exp))
      (concatenacion-exp (operando cadena1 cadena2)
                         (type-of-application
                          (type-of-primitive4 operando)
                          (types-of-expressions
                           (list cadena1 cadena2) tenv)
                          operando (list cadena1 cadena2) exp))
      (negacion-exp (operando boolean)
                    (type-of-application
                     (type-of-primitive5 operando)
                     (types-of-expressions
                      (list boolean) tenv)
                     operando (list boolean) exp))
      (procedimiento-exp (nombrefuncion listatipos parametros cuerpo) int-type)
      (invocacion-proc-exp (nombrefuncion argumentos)
;                           (type-of-application
;                            (evaluar-tipo-expresion nombrefuncion tenv)
;                            (types-of-expressions argumentos tenv)
;                            nombrefuncion argumentos exp)
                           int-type)
      (procedimiento-rec-exp (nombre-funcion listatipos parametros cuerpo) int-type)
      (invocacion-proc-rec-exp (nombre-funcion argumentos) int-type)
      (estructura-exp (identificador listatipos lista-exp) int-type)
      )))

;; Función que realiza la secuenciación con tipos
(define secuenciacion-tipos
  (lambda (lista tenv)
    (if (null? (cdr lista))
        (evaluar-tipo-expresion (car lista) tenv)
        (let
            (
             (expresion-eval (evaluar-tipo-expresion (car lista) tenv))
             )
          (if (type? expresion-eval)
              (secuenciacion-tipos (cdr lista) tenv)
              (eopl:error "error en la secuenciación"))
          )
        )))

;; Función que determina si el valor de una variable corresponde con el tipo que de declara
(define tipo-variables
  (lambda (listatipos valores tenv exp)
    (let
        (
         (tipos-eval (expand-type-expressions listatipos))
         (valores-eval (map (lambda (x) (evaluar-tipo-expresion x tenv)) valores))
         )
      (verificar-tipos tipos-eval valores-eval exp)
      )))

(define verificar-tipos
  (lambda (tipos valores exp)
    (cond
      [(null? tipos) #t]
      [(and (check-equal-type! (car tipos) (car valores) exp) (verificar-tipos (cdr tipos) (cdr valores) exp))]      
      )))

;; Función que busca el tipo de un identificador en un ambiente de tipos
;(define apply-tenv 
;  (lambda (tenv sym)
;    (cases ambiente-tipos tenv
;      (empty-tenv ()
;                  (eopl:error 'apply-tenv "Unbound variable ~s" sym))
;      (extended-tenv (syms vals env)
;                     (let ((pos (list-find-position sym syms)))
;                       (if (number? pos)
;                           (list-ref vals pos)
;                           (apply-tenv env sym)))))))

;; Definición de los tipos para las variables
(define int-type
  (atomic-type 'int))

(define float-type
  (atomic-type 'float))

(define hexadecimal-type
  (atomic-type 'hexadecimal))

(define octal-type
  (atomic-type 'octal))

(define string-type
  (atomic-type 'string))

(define bool-type
  (atomic-type 'bool))

;; Función que determina el tipo de una variable
(define expand-type-expression
  (lambda (texp)
    (cases type-exp texp
      (int-type-exp () int-type)
      (float-type-exp () float-type)
      (hexadecimal-type-exp () hexadecimal-type)
      (octal-type-exp () octal-type)
      (string-type-exp () string-type)
      (bool-type-exp () bool-type)
      (proc-type-exp (arg-texps result-texp)
                     (proc-type
                      (expand-type-expressions arg-texps)
                      (expand-type-expression result-texp))))))

;; Función que le aplica expand-type-expression a una lista y retorna una lista de tipos
(define expand-type-expressions
  (lambda (texps)
    (map expand-type-expression texps)))

;; Función que verifica si dos tipos son iguales. Muestra un mensaje de error en caso de que no lo sean
(define check-equal-type!
  (lambda (t1 t2 exp)
    (if (not (equal? t1 t2))
        (eopl:error 'check-equal-type!
                    "Los tipos no coinciden. Se está pasando un ~s y se espera un ~s en~%~s"
                    (type-to-external-form t1)
                    (type-to-external-form t2)
                    exp)
        #t)))

;; Función que recibe un tipo y devuelve una representación del tipo facil de leer
(define type-to-external-form
  (lambda (ty)
    (cases type ty
      (atomic-type (name) name)
      (proc-type (arg-types result-type)
                 (append
                  (arg-types-to-external-form arg-types)
                  '(->)
                  (list (type-to-external-form result-type)))))))

(define arg-types-to-external-form
  (lambda (types)
    (if (null? types)
        '()
        (if (null? (cdr types))
            (list (type-to-external-form (car types)))
            (cons
             (type-to-external-form (car types))
             (cons '*
                   (arg-types-to-external-form (cdr types))))))))

;; Función que determina el tipo de una expresión de aplicación
(define type-of-application
  (lambda (rator-type rand-types rator rands exp)
    (cases type rator-type
      (proc-type (arg-types result-type)
                 (if (= (length arg-types) (length rand-types))
                     (begin
                       (for-each
                        check-equal-type!
                        rand-types arg-types rands)
                       result-type)
                     (eopl:error 'type-of-expression
                                 (string-append
                                  "El número de argumentos no corresponde con los recibidos en ~s:"
                                  "~%experados ~s~%pasados ~s")
                                 exp
                                 (map type-to-external-form arg-types)
                                 (map type-to-external-form rand-types))))
      (else
       (eopl:error 'type-of-expression
                   "Rator not a proc type:~%~s~%had rator type ~s"
                   rator (type-to-external-form rator-type))))))


;; Función que determina el tipo de una primitiva entera
(define type-of-primitive-int
  (lambda (prim)
    (cases primitiva prim
      (suma-prim () (proc-type (or (list int-type int-type) (list float-type float-type)) (or int-type float-type)))
      (resta-prim () (proc-type (list int-type int-type) int-type))
      (multiplicacion-prim () (proc-type (list int-type int-type) int-type))
      (division-prim () (proc-type (list int-type int-type) (or int-type float-type)))
      (modulo-prim () (proc-type (list int-type int-type) int-type))
      (menor-prim () (proc-type (list int-type int-type) bool-type))
      (mayor-prim () (proc-type (list int-type int-type) bool-type))
      (menor-igual-prim () (proc-type (list int-type int-type) bool-type))
      (mayor-igual-prim () (proc-type (list int-type int-type) bool-type))
      (igual-prim () (proc-type (list int-type int-type) bool-type))
      (diferente-prim () (proc-type (list int-type int-type) bool-type))
      (conjuncion-prim () (proc-type (list bool-type bool-type) bool-type))
      (disyuncion-prim () (proc-type (list bool-type bool-type) bool-type))
      )))

;; Función que determina el tipo de una primitiva flotante
(define type-of-primitive-float
  (lambda (prim)
    (cases primitiva prim
      (suma-prim () (proc-type (or (list float-type float-type) (list float-type float-type)) (or float-type float-type)))
      (resta-prim () (proc-type (list float-type float-type) float-type))
      (multiplicacion-prim () (proc-type (list float-type float-type) float-type))
      (division-prim () (proc-type (list float-type float-type) (or int-type float-type)))
      (modulo-prim () (proc-type (list float-type float-type) float-type))
      (menor-prim () (proc-type (list float-type float-type) bool-type))
      (mayor-prim () (proc-type (list float-type float-type) bool-type))
      (menor-igual-prim () (proc-type (list float-type float-type) bool-type))
      (mayor-igual-prim () (proc-type (list float-type float-type) bool-type))
      (igual-prim () (proc-type (list float-type float-type) bool-type))
      (diferente-prim () (proc-type (list float-type float-type) bool-type))
      (conjuncion-prim () (proc-type (list bool-type bool-type) bool-type))
      (disyuncion-prim () (proc-type (list bool-type bool-type) bool-type))
      )))

;; Función que determina el tipo de una primitiva hexadecimal
(define type-of-primitive-hexadecimal
  (lambda (prim)
    (cases primitiva prim
      (suma-prim () (proc-type (or (list hexadecimal-type hexadecimal-type) (list hexadecimal-type hexadecimal-type)) (or hexadecimal-type hexadecimal-type)))
      (resta-prim () (proc-type (list hexadecimal-type hexadecimal-type) hexadecimal-type))
      (multiplicacion-prim () (proc-type (list hexadecimal-type hexadecimal-type) hexadecimal-type))
      (division-prim () (proc-type (list hexadecimal-type hexadecimal-type) hexadecimal-type))
      (modulo-prim () (proc-type (list hexadecimal-type hexadecimal-type) hexadecimal-type))
      (menor-prim () (proc-type (list hexadecimal-type hexadecimal-type) bool-type))
      (mayor-prim () (proc-type (list hexadecimal-type hexadecimal-type) bool-type))
      (menor-igual-prim () (proc-type (list hexadecimal-type hexadecimal-type) bool-type))
      (mayor-igual-prim () (proc-type (list hexadecimal-type hexadecimal-type) bool-type))
      (igual-prim () (proc-type (list hexadecimal-type hexadecimal-type) bool-type))
      (diferente-prim () (proc-type (list hexadecimal-type hexadecimal-type) bool-type))
      (conjuncion-prim () (proc-type (list bool-type bool-type) bool-type))
      (disyuncion-prim () (proc-type (list bool-type bool-type) bool-type))
      )))

;; Función que determina el tipo de una primitiva octal
(define type-of-primitive-octal
  (lambda (prim)
    (cases primitiva prim
      (suma-prim () (proc-type (or (list octal-type octal-type) (list octal-type octal-type)) (or octal-type octal-type)))
      (resta-prim () (proc-type (list octal-type octal-type) octal-type))
      (multiplicacion-prim () (proc-type (list octal-type octal-type) octal-type))
      (division-prim () (proc-type (list octal-type octal-type) octal-type))
      (modulo-prim () (proc-type (list octal-type octal-type) octal-type))
      (menor-prim () (proc-type (list octal-type octal-type) bool-type))
      (mayor-prim () (proc-type (list octal-type octal-type) bool-type))
      (menor-igual-prim () (proc-type (list octal-type octal-type) bool-type))
      (mayor-igual-prim () (proc-type (list octal-type octal-type) bool-type))
      (igual-prim () (proc-type (list octal-type octal-type) bool-type))
      (diferente-prim () (proc-type (list octal-type octal-type) bool-type))
      (conjuncion-prim () (proc-type (list bool-type bool-type) bool-type))
      (disyuncion-prim () (proc-type (list bool-type bool-type) bool-type))
      )))

;; Función que determina el tipo de una primitiva2
(define type-of-primitive2
  (lambda (prim)
    (cases primitiva2 prim
      (incremento-prim () (proc-type (list int-type) int-type))
      (decremento-prim () (proc-type (list int-type) int-type))
      )))

;; Función que determina el tipo de una primitiva3
(define type-of-primitive3
  (lambda (prim)
    (cases primitiva3 prim
      (longitud-prim () (proc-type (list string-type) int-type))
      )))

;; Función que determina el tipo de una primitiva4
(define type-of-primitive4
  (lambda (prim)
    (cases primitiva4 prim
      (concatenacion-prim () (proc-type (list string-type string-type) string-type))
      )))

;; Función que determina el tipo de una primitiva5
(define type-of-primitive5
  (lambda (prim)
    (cases primitiva5 prim
      (negacion-prim () (proc-type (list bool-type) bool-type))
      )))

;; Función que mapea la función type-of-expresion a una lista
(define types-of-expressions
  (lambda (rands tenv)
    (map (lambda (exp) (evaluar-tipo-expresion exp tenv)) rands)))

;; Función que determina el tipo de un procedimiento
(define type-of-proc-exp
  (lambda (nombrefuncion texps ids body tenv)
    (let ((arg-types (expand-type-expressions texps)))
      (let ((result-type
             (evaluar-tipo-expresion body (extended-tenv nombrefuncion ids arg-types tenv))))
        (proc-type arg-types result-type)))))

;; Ejecución del interpretador
;(interpretador)

;; Ejecución del interpretador de tipos
(interpretador-tipos)
