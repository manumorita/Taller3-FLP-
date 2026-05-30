#lang eopl

; ============================================================
; TALLER 3 - FUNDAMENTOS DE LENGUAJES DE PROGRAMACIÓN
; ============================================================
; Repositorio: https://github.com/manumorita/Taller3-FLP-.git
; Integrantes:
; - Manuela Martínez Moncada - 2375458
; - Steven Fernando Aragón Álvarez - 2418804
; - Andrés Gerardo González Rosero - 2416541
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
    ; textos: debe iniciar con letra o '_' seguido de letras, dígitos o '_'
    (texto
      (letter (arbno (or letter digit "_" ":")))
      string)

    (texto
      ("_" (arbno (or letter digit "_" ":")))
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
    (primitive-bin ("==") primitiva-comparador-igual)

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
; AMBIENTES
; ============================================================

; datatype para representar ambientes

(define-datatype ambiente ambiente?

  (vacio)

  (extendido
   (ids (list-of symbol?))
   (vals (list-of scheme-value?))
   (amb ambiente?))

  (extendido-recursivo
   (proc-nombres (list-of symbol?))
   (lista-ids (list-of list?))
   (proc-cuerpos (list-of expression?))
   (amb ambiente?)))

; ============================================================
; PROCEDIMIENTOS
; ============================================================

; datatype para representar cerraduras

(define-datatype procVal procVal?

  (cerradura
   (lista-ID (list-of symbol?))
   (exp expression?)
   (amb ambiente?)))

; ============================================================
; FUNCIÓN AUXILIAR
; ============================================================

; determina si un valor es válido dentro del interpretador

(define scheme-value?

  (lambda (v)
    #t))

; ============================================================
; AMBIENTE INICIAL
; ============================================================

; ambiente base solicitado en el taller

(define ambiente-inicial

  (extendido
   '(@a @b @c @d @e)
   '(1 2 3 "hola" "FLP")
   (vacio)))

; ============================================================
; BÚSQUEDA DE VARIABLES
; ============================================================

; busca una variable dentro del ambiente

(define buscar-variable

  (lambda (id amb)

    (cases ambiente amb

      (vacio ()
             "Error, la variable no existe")

      (extendido (ids vals old-amb)

                  (let ((pos (encontrar-posicion id ids)))

                    (if pos
                        (list-ref vals pos)
                        (buscar-variable id old-amb))))

      (extendido-recursivo
       (proc-nombres lista-ids proc-cuerpos old-amb)

       (let ((pos (encontrar-posicion id proc-nombres)))

         (if pos

             (cerradura
              (list-ref lista-ids pos)
              (list-ref proc-cuerpos pos)
              amb)

             (buscar-variable id old-amb)))))))

; ============================================================
; ENCONTRAR POSICIÓN
; ============================================================

; retorna la posición de un elemento dentro de una lista

(define encontrar-posicion

  (lambda (elem lista)

    (let loop ((lista lista)
               (indice 0))

      (cond

        [(null? lista)
         #f]

        [(equal? elem (car lista))
         indice]

        [else
         (loop (cdr lista) (+ indice 1))]))))

; ============================================================
; VALOR VERDAD
; ============================================================

; determina si un valor es verdadero o falso

(define valor-verdad?

  (lambda (valor)

    (if (equal? valor 0)
        #f
        #t)))

; ============================================================
; EVALUADOR PRINCIPAL
; ============================================================

; evalúa expresiones del lenguaje

(define evaluar-expresion

  (lambda (exp amb)

    (cases expression exp

      ; ======================================================
      ; LITERALES
      ; ======================================================

      (numero-lit (num)
                   num)

      (texto-lit (txt)
                  txt)

      ; ======================================================
      ; VARIABLES
      ; ======================================================

      (var-exp (id)
               (buscar-variable id amb))

      ; ======================================================
      ; PRIMITIVAS BINARIAS
      ; ======================================================

      (primapp-bin-exp (exp1 prim exp2)

                       (evaluar-primitiva-binaria
                        prim
                        (evaluar-expresion exp1 amb)
                        (evaluar-expresion exp2 amb)))

      ; ======================================================
      ; PRIMITIVAS UNARIAS
      ; ======================================================

      (primapp-un-exp (prim exp1)

                      (evaluar-primitiva-unaria
                       prim
                       (evaluar-expresion exp1 amb)))

      ; ======================================================
      ; CONDICIONALES
      ; ======================================================

      (condicional-exp (test-exp true-exp false-exp)

                       (if (valor-verdad?
                            (evaluar-expresion test-exp amb))

                           (evaluar-expresion true-exp amb)

                           (evaluar-expresion false-exp amb)))

      ; ======================================================
      ; VARIABLES LOCALES
      ; ======================================================

      (variableLocal-exp (ids exps cuerpo)

                         (let ((vals
                                (map
                                 (lambda (exp)
                                   (evaluar-expresion exp amb))
                                 exps)))

                           (evaluar-expresion
                            cuerpo
                            (extendido ids vals amb))))

      ; ======================================================
      ; PROCEDIMIENTOS
      ; ======================================================

      (procedimiento-exp (ids cuerpo)

                         (cerradura ids cuerpo amb))

      ; ======================================================
      ; APLICACIÓN DE PROCEDIMIENTOS
      ; ======================================================

      (app-exp (rator rands)

               (let ((proc (evaluar-expresion rator amb))
                     (args
                      (map
                       (lambda (exp)
                         (evaluar-expresion exp amb))
                       rands)))

                 (aplicar-procedimiento proc args)))

      ; ======================================================
      ; RECURSIÓN
      ; ======================================================

      (recursivo-exp
       (proc-nombres lista-ids proc-cuerpos cuerpo)

       (evaluar-expresion
        cuerpo

        (extendido-recursivo
         proc-nombres
         lista-ids
         proc-cuerpos
         amb))))))

; ============================================================
; APLICAR PROCEDIMIENTO
; ============================================================

; aplica una cerradura con sus argumentos

(define aplicar-procedimiento

  (lambda (proc args)

    (cases procVal proc

      (cerradura (ids cuerpo amb)

                  (evaluar-expresion
                   cuerpo

                   (extendido
                    ids
                    args
                    amb))))))

; ============================================================
; PRIMITIVAS BINARIAS
; ============================================================

; evalúa primitivas binarias

(define evaluar-primitiva-binaria

  (lambda (prim arg1 arg2)

    (cases primitive-bin prim

      (primitiva-suma ()
                       (+ arg1 arg2))

      (primitiva-resta ()
                        (- arg1 arg2))

      (primitiva-multi ()
                        (* arg1 arg2))

      (primitiva-div ()
                      (/ arg1 arg2))

      (primitiva-concat ()
                         (string-append arg1 arg2))

      (primitiva-mayor ()
                        (if (> arg1 arg2) 1 0))

      (primitiva-menor ()
                        (if (< arg1 arg2) 1 0))

      (primitiva-mayor-igual ()
                              (if (>= arg1 arg2) 1 0))

      (primitiva-menor-igual ()
                              (if (<= arg1 arg2) 1 0))

      (primitiva-diferente ()
                             (if (not (equal? arg1 arg2)) 1 0))

      (primitiva-comparador-igual ()
                              (if (equal? arg1 arg2) 1 0)))))

; ============================================================
; PRIMITIVAS UNARIAS
; ============================================================

; evalúa primitivas unarias

(define evaluar-primitiva-unaria

  (lambda (prim arg)

    (cases primitive-un prim

      (primitiva-longitud ()
                           (string-length arg))

      (primitiva-add1 ()
                        (+ arg 1))

      (primitiva-sub1 ()
                        (- arg 1))

      (primitiva-neg ()
                      (if (valor-verdad? arg) 0 1))

      (primitiva-piso ()
                       (floor arg)))))

; ============================================================
; EVALUAR PROGRAMA
; ============================================================

; evalúa un programa completo

(define evaluar-programa

  (lambda (pgm)

    (cases program pgm

      (un-programa (exp)

                   (evaluar-expresion
                    exp
                    ambiente-inicial)))))

; ============================================================
; INTERFAZ
; ============================================================

; ejecuta el scanner, parser y evaluador

(define interpretar

  (lambda (texto)

    (evaluar-programa
     (scan&parse texto))))

; Para mayor comodidad, se definen sinónimos para la función interpretar (gusto personal)
(define interprete interpretar)
(define interpretador interpretar)
(provide (all-defined-out))

; ============================================================
; EJERCICIOS FINALES DEL TALLER (COMENTADOS)
; ============================================================
;
; 9a) sumarDigitos
; (interpretar "recursivo (@sumarDigitos(@n)=
;    Si (@n < 10) {
;      @n
;    } sino {
;      ((@n ~ (piso((@n / 10)) * 10)) + evaluar @sumarDigitos(piso((@n / 10))) finEval)
;    }
;  ;) { evaluar @sumarDigitos(147) finEval }")
;
; 9b) factorial
; (interpretar "recursivo (@fact(@n)=
;    Si (@n <= 1) {
;      1
;    } sino {
;      (@n * evaluar @fact((@n ~ 1)) finEval)
;    }
;  ;) { evaluar @fact(5) finEval }")
;
; (interpretar "recursivo (@fact(@n)=
;    Si (@n <= 1) {
;      1
;    } sino {
;      (@n * evaluar @fact((@n ~ 1)) finEval)
;    }
;  ;) { evaluar @fact(10) finEval }")
;
; 9c) potencia recursiva
; (interpretar "recursivo (@potencia(@base,@exp)=
;    Si (@exp <= 0) {
;      1
;    } sino {
;      (@base * evaluar @potencia(@base,(@exp ~ 1)) finEval)
;    }
;  ;) { evaluar @potencia(4,2) finEval }")
;
; 9d) suma de rango
; (interpretar "recursivo (@sumaRango(@a,@b)=
;    Si (@a == @b) {
;      @a
;    } sino {
;      (@a + evaluar @sumaRango((@a + 1),@b) finEval)
;    }
;  ;) { evaluar @sumaRango(2,5) finEval }")
;
; 9e) decorador sin mensaje final
; (interpretar "declarar (
;   @integrantes = procedimiento () { \"Manuela_Steven_Andres\" };
;   @saludar = procedimiento (@proc) {
;     procedimiento () { (\"Hola:\" concat evaluar @proc () finEval) }
;   };
; ) {
;   declarar (
;     @decorate = evaluar @saludar (@integrantes) finEval;
;   ) {
;     evaluar @decorate () finEval
;   }
; }")
;
; 9f) decorador con mensaje final
; (interpretar "declarar (
;   @integrantes = procedimiento () { \"Manuela_Steven_Andres\" };
;   @saludar = procedimiento (@proc) {
;     procedimiento (@mensaje) {
;       ((\"Hola:\" concat evaluar @proc () finEval) concat @mensaje)
;     }
;   };
; ) {
;   declarar (
;     @decorate = evaluar @saludar (@integrantes) finEval;
;   ) {
;     evaluar @decorate (\"ProfesoresFLP\") finEval
;   }
; }")