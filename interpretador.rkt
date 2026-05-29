#lang eopl

; ============================================================
; TALLER 3 - FUNDAMENTOS DE LENGUAJES DE PROGRAMACIÓN
; ============================================================
; Integrantes:
; - Manuela Martinez Moncada
; - Steven Aragon 
; - Gerardo Gonzales
;
; ============================================================
; ESPECIFICACIÓN LÉXICA
; ============================================================

(define scanner-spec
  '(
    
    ; espacios
    (white-sp
      (whitespace)
      skip)

    ; comentarios
    (comment
      ("//" (arbno (not #\newline)))
      skip)

    ; identificadores
    (identifier
      ("@" letter (arbno (or letter digit "_")))
      symbol)

    ; enteros positivos
    (number
      (digit (arbno digit))
      number)

    ; enteros negativos
    (number
      ("-" digit (arbno digit))
      number)

    ; decimales positivos
    (number
      (digit (arbno digit) "." digit (arbno digit))
      number)

    ; decimales negativos
    (number
      ("-" digit (arbno digit) "." digit (arbno digit))
      number)

    ; textos
    (texto
      (letter (arbno (or letter digit "_" ":")))
      string)
))

; ============================================================
; GRAMÁTICA
; ============================================================

(define grammar

  '(

    ; ========================================================
    ; PROGRAMA
    ; ========================================================

    (program
      (expression)
      un-programa)

    ; ========================================================
    ; EXPRESIONES BÁSICAS
    ; ========================================================

    ; números
    (expression
      (number)
      numero-lit)

    ; textos
    (expression
      ("\"" texto "\"")
      texto-lit)

    ; variables
    (expression
      (identifier)
      var-exp)

    ; ========================================================
    ; PRIMITIVAS BINARIAS
    ; ========================================================

    ; (exp + exp)

    (expression
      ("(" expression primitive-bin expression ")")
      primapp-bin-exp)

    ; ========================================================
    ; PRIMITIVAS UNARIAS
    ; ========================================================

    ; longitud(exp)

    (expression
      (primitive-un "(" expression ")")
      primapp-un-exp)

    ; ========================================================
    ; CONDICIONALES
    ; ========================================================

    (expression
      ("Si"
       expression
       "{"
       expression
       "}"
       "sino"
       "{"
       expression
       "}")
      condicional-exp)

    ; ========================================================
    ; VARIABLES LOCALES
    ; ========================================================

    (expression
      ("declarar"
       "("
       (arbno identifier "=" expression ";")
       ")"
       "{"
       expression
       "}")
      variableLocal-exp)

    ; ========================================================
    ; PROCEDIMIENTOS
    ; ========================================================

    (expression
      ("procedimiento"
       "("
       (separated-list identifier ",")
       ")"
       "{"
       expression
       "}")
      procedimiento-exp)

    ; ========================================================
    ; APLICACIÓN
    ; ========================================================

    (expression
      ("evaluar"
       expression
       "("
       (separated-list expression ",")
       ")"
       "finEval")
      app-exp)

    ; ========================================================
    ; RECURSIÓN
    ; ========================================================

    (expression
      ("recursivo"

       "("

       (arbno

        identifier

        "("
        (separated-list identifier ",")
        ")"

        "="

        expression

        ";")

       ")"

       "{"

       expression

       "}")

      recursivo-exp)

    ; ========================================================
    ; PRIMITIVAS BINARIAS
    ; ========================================================

    (primitive-bin ("+") primitiva-suma)
    (primitive-bin ("~") primitiva-resta)
    (primitive-bin ("*") primitiva-multi)
    (primitive-bin ("/") primitiva-div)

    (primitive-bin ("concat") primitiva-concat)

    (primitive-bin (">") primitiva-mayor)
    (primitive-bin ("<") primitiva-menor)

    (primitive-bin (">=") primitiva-mayor-igual)
    (primitive-bin ("<=") primitiva-menor-igual)

    (primitive-bin ("!=") primitiva-diferente)
    (primitive-bin ("==") primitiva-igual)

    ; ========================================================
    ; PRIMITIVAS UNARIAS
    ; ========================================================

    (primitive-un ("longitud") primitiva-longitud)
    (primitive-un ("add1") primitiva-add1)
    (primitive-un ("sub1") primitiva-sub1)
    (primitive-un ("neg") primitiva-neg)
    (primitive-un ("piso") primitiva-piso)
))

; ============================================================
; DATATYPES AUTOMÁTICOS
; ============================================================

(sllgen:make-define-datatypes
  scanner-spec
  grammar)

; ============================================================
; PARSER
; ============================================================

(define scan&parse
  (sllgen:make-string-parser
    scanner-spec
    grammar))


; ============================================================
; DATATYPES DE AMBIENTE
; ============================================================

; Un ambiente es una lista enlazada de marcos.
; vacio       → el ambiente base (no tiene variables)
; extendido   → un marco con una lista de ids, una lista de
;               valores, y un puntero al ambiente anterior

(define-datatype ambiente ambiente?
  (vacio)
  (extendido
    (ids    (list-of symbol?))   ; lista de identificadores
    (vals   (list-of scheme-value?))  ; lista de valores asociados
    (resto  ambiente?)))          ; ambiente anterior (encadenado)

; scheme-value? acepta cualquier valor de Racket/EOPL
(define scheme-value? (lambda (v) #t))

; ============================================================
; FUNCIONES DE AMBIENTE
; ============================================================

; ambiente-vacio: -> ambiente
; Retorna el ambiente base sin variables.
(define ambiente-vacio
  (lambda ()
    (vacio)))

; extender-ambiente: (list-of symbol?) (list-of valor) ambiente -> ambiente
; Crea un nuevo marco con las variables ids y valores vals
; encadenado al ambiente anterior amb.
(define extender-ambiente
  (lambda (ids vals amb)
    (extendido ids vals amb)))

; buscar-variable: symbol ambiente -> valor
; Recorre la cadena de marcos buscando el símbolo id.
; Si lo encuentra retorna su valor; si llega a vacio, error.
(define buscar-variable
  (lambda (id amb)
    (cases ambiente amb
      (vacio ()
        (error 'buscar-variable "Error, la variable no existe: ~s" id))
      (extendido (ids vals resto)
        (let buscar-en-marco ((ids-restantes ids)
                              (vals-restantes vals))
          (cond
            ; llegamos al final del marco sin encontrarla → subir al padre
            ((null? ids-restantes)
             (buscar-variable id resto))
            ; la encontramos → retornar su valor
            ((eqv? (car ids-restantes) id)
             (car vals-restantes))
            ; no es esta → seguir en el mismo marco
            (else
             (buscar-en-marco (cdr ids-restantes)
                              (cdr vals-restantes)))))))))

; ============================================================
; AMBIENTE INICIAL
; ============================================================

; ambiente-init: ambiente
; Ambiente base con las 5 variables del enunciado.
(define ambiente-init
  (extender-ambiente
    '(@a @b @c @d @e)
    '(1   2   3  "hola" "FLP")
    (ambiente-vacio)))

; ============================================================
; DATATYPE PROCVAL (CERRADURA)
; ============================================================

; Un procVal representa un procedimiento como valor de primera clase.
; Tiene 3 campos:
;   lista-ID  → los parámetros formales del procedimiento
;   exp       → el cuerpo (una expresion del AST)
;   amb       → el ambiente donde fue declarado (para closures)

(define-datatype procVal procVal?
  (cerradura
    (lista-ID (list-of symbol?))
    (exp expresion?)
    (amb ambiente?)))

; ============================================================
; EVALUADOR
; ============================================================

; evaluar-programa: programa -> valor
; Punto de entrada principal. Extrae la expresion del programa
; y la evalua en el ambiente inicial.
(define evaluar-programa
  (lambda (pgm)
    (cases program pgm
      (un-programa (exp)
        (evaluar-expresion exp ambiente-init)))))

; evaluar-expresion: expresion ambiente -> valor
; Nucleo del interprete. Despacha segun el tipo de nodo del AST.
(define evaluar-expresion
  (lambda (exp amb)
    (cases expression exp

      ; ----------------------------------------------------------
      ; LITERALES
      ; ----------------------------------------------------------

      ; numero-lit: retorna el numero directamente
      (numero-lit (num) num)

      ; texto-lit: retorna el string directamente
      (texto-lit (txt) txt)

      ; var-exp: busca el identificador en el ambiente
      (var-exp (id)
        (buscar-variable id amb))

      ; ----------------------------------------------------------
      ; PRIMITIVAS BINARIAS
      ; ----------------------------------------------------------

      ; primapp-bin-exp: evalua ambos operandos y aplica la primitiva
      (primapp-bin-exp (exp1 prim exp2)
        (let ((val1 (evaluar-expresion exp1 amb))
              (val2 (evaluar-expresion exp2 amb)))
          (aplicar-primitiva-bin prim val1 val2)))

      ; ----------------------------------------------------------
      ; PRIMITIVAS UNARIAS
      ; ----------------------------------------------------------

      ; primapp-un-exp: evalua el operando y aplica la primitiva
      (primapp-un-exp (prim exp1)
        (let ((val (evaluar-expresion exp1 amb)))
          (aplicar-primitiva-un prim val)))

      ; ----------------------------------------------------------
      ; CONDICIONAL
      ; ----------------------------------------------------------

      ; condicional-exp: evalua test, si es verdadero evalua true-exp,
      ; de lo contrario evalua false-exp
      (condicional-exp (test-exp true-exp false-exp)
        (if (valor-verdad? (evaluar-expresion test-exp amb))
            (evaluar-expresion true-exp amb)
            (evaluar-expresion false-exp amb)))

      ; ----------------------------------------------------------
      ; VARIABLES LOCALES
      ; ----------------------------------------------------------

      ; variableLocal-exp: evalua cada expresion de inicializacion,
      ; extiende el ambiente con los nuevos enlaces y evalua el cuerpo
      (variableLocal-exp (ids exps cuerpo)
        (let ((vals (map (lambda (e) (evaluar-expresion e amb)) exps)))
          (evaluar-expresion cuerpo
                             (extender-ambiente ids vals amb))))

      ; ----------------------------------------------------------
      ; PROCEDIMIENTOS
      ; ----------------------------------------------------------

      ; procedimiento-exp: construye y retorna una cerradura,
      ; capturando el ambiente actual
      (procedimiento-exp (ids cuerpo)
        (cerradura ids cuerpo amb))

      ; ----------------------------------------------------------
      ; APLICACION DE PROCEDIMIENTO
      ; ----------------------------------------------------------

      ; app-exp: evalua el procedimiento y los argumentos,
      ; luego aplica la cerradura
      (app-exp (exp-proc exps-args)
        (let ((proc (evaluar-expresion exp-proc amb))
              (args (map (lambda (e) (evaluar-expresion e amb)) exps-args)))
          (aplicar-cerradura proc args)))

      ; ----------------------------------------------------------
      ; RECURSION
      ; ----------------------------------------------------------

      ; recursivo-exp: construye un ambiente recursivo donde cada
      ; procedimiento puede verse a si mismo y a los demas,
      ; luego evalua el cuerpo en ese ambiente
      (recursivo-exp (ids params exps cuerpo)
        (evaluar-expresion cuerpo
                           (extender-ambiente-recursivo ids params exps amb)))

    )))

; ============================================================
; APLICAR CERRADURA
; ============================================================

; aplicar-cerradura: procVal (list-of valor) -> valor
; Extiende el ambiente de la cerradura con los argumentos
; y evalua el cuerpo en ese nuevo ambiente.
(define aplicar-cerradura
  (lambda (proc args)
    (cases procVal proc
      (cerradura (ids cuerpo amb-declaracion)
        (evaluar-expresion cuerpo
                           (extender-ambiente ids args amb-declaracion))))))

; ============================================================
; VALOR-VERDAD?
; ============================================================

; valor-verdad?: valor -> boolean
; En este lenguaje 0 es falso, cualquier otro valor es verdadero.
(define valor-verdad?
  (lambda (val)
    (not (equal? val 0))))

; ============================================================
; AMBIENTE RECURSIVO
; ============================================================

; extender-ambiente-recursivo: ids params exps ambiente -> ambiente
; Construye cerraduras para cada procedimiento recursivo y las
; enlaza en un nuevo ambiente donde todas se ven entre si.
; El truco: todas las cerraduras apuntan al mismo ambiente extendido,
; que se construye de forma diferida con letrec.
(define extender-ambiente-recursivo
  (lambda (ids params exps amb)
    (letrec
      ((amb-rec
        (extender-ambiente
          ids
          (map (lambda (ps cuerpo)
                 (cerradura ps cuerpo amb-rec))
               params
               exps)
          amb)))
      amb-rec)))

; ============================================================
; PRIMITIVAS BINARIAS
; ============================================================

; aplicar-primitiva-bin: primitive-bin valor valor -> valor
; Despacha segun el tipo de primitiva binaria y aplica
; la operacion correspondiente sobre val1 y val2.
(define aplicar-primitiva-bin
  (lambda (prim val1 val2)
    (cases primitive-bin prim

      ; aritmeticas
      (primitiva-suma  () (+ val1 val2))
      (primitiva-resta () (- val1 val2))
      (primitiva-multi () (* val1 val2))
      (primitiva-div   () (/ val1 val2))

      ; strings
      ; concat: ambos operandos deben ser strings
      (primitiva-concat ()
        (string-append val1 val2))

      ; comparadores numericos → retornan 1 (verdadero) o 0 (falso)
      (primitiva-mayor       () (if (> val1 val2)  1 0))
      (primitiva-menor       () (if (< val1 val2)  1 0))
      (primitiva-mayor-igual () (if (>= val1 val2) 1 0))
      (primitiva-menor-igual () (if (<= val1 val2) 1 0))
      (primitiva-diferente   () (if (not (equal? val1 val2)) 1 0))
      (primitiva-igual       () (if (equal? val1 val2) 1 0))
    )))

; ============================================================
; PRIMITIVAS UNARIAS
; ============================================================

; aplicar-primitiva-un: primitive-un valor -> valor
; Despacha segun el tipo de primitiva unaria y aplica
; la operacion correspondiente sobre val.
(define aplicar-primitiva-un
  (lambda (prim val)
    (cases primitive-un prim

      ; longitud: funciona sobre strings y listas
      (primitiva-longitud ()
        (cond
          ((string? val) (string-length val))
          ((list? val)   (length val))
          (else (error 'longitud "Se esperaba string o lista, se recibio: ~s" val))))

      ; add1/sub1: incremento y decremento
      (primitiva-add1 () (+ val 1))
      (primitiva-sub1 () (- val 1))

      ; neg: negacion booleana → 0 si verdadero, 1 si falso
      (primitiva-neg ()
        (if (valor-verdad? val) 0 1))

      ; piso: parte entera de un decimal
      (primitiva-piso ()
        (floor val))
    )))

; ============================================================
; INTERPRETE
; ============================================================

; interprete: string -> valor
; Funcion de entrada: recibe codigo como string,
; lo parsea y lo evalua.
(define interprete
  (lambda (codigo)
    (evaluar-programa (scan&parse codigo))))