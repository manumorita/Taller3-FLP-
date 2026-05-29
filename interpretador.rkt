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