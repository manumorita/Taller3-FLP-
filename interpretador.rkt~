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
