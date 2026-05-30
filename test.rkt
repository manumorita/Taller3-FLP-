#lang racket

(require (file "interpretador.rkt"))

(define (run label prog)
  (displayln "------------------------------")
  (displayln (string-append "Prueba: " label))
  (displayln (string-append "Programa: " prog))
  (with-handlers ([exn:fail? (lambda (e) (displayln (string-append "ERROR: " (exn-message e))))])
    (let ((res (interpretador prog)))
      (displayln (string-append "Resultado: "))
      (displayln res))))

;; Tests básicos de variables y booleanos
(run "@a" "@a")
(run "@b" "@b")
(run "@e" "@e")
(run "(5 > 9)" "(5 > 9)")
(run "(6 <= 6)" "(6 <= 6)")

;; Condicionales
(run "Si (2+3) {2} sino {3}" "Si (2+3) { 2 }sino{ 3 }")
(run "Si (longitud(@d) ~ 4) {2} sino {3}"
     "Si (longitud(@d) ~ 4) { 2 } sino { 3 }")

;; Declarar variables locales
(run "declarar local - ejemplo 1"
     "declarar (@x=2;@y=3;@a=7;){ (@a+(@x~@y)) }")
(run "declarar local - ejemplo 2"
     "declarar (@x=2;@y=3;@a=7;) { (@a+@b) }")

;; Procedimiento y aplicación simple
(run "procedimiento (ejemplo)"
     "procedimiento (@x,@y,@z) { ((@x+@y)+@z) }")

;; Evaluación de procedimientos (ejemplo provisto)
(run "evaluar procedimiento - ejemplo 1"
     "declarar (\n      @x=2;\n      @y=3;\n      @a=procedimiento (@x,@y,@z) { ((@x+@y)+@z) } ;\n     ) { \n         evaluar @a (1,2,@x) finEval  \n       }")

(run "evaluar procedimiento - ejemplo 2"
     "declarar (\n     @x=procedimiento (@a,@b) { ((@a*@a) + (@b*@b)) };\n     @y=procedimiento (@x,@y) { (@x+@y) } ;\n    ) { \n      ( evaluar @x(1,2) finEval + evaluar @y(2,3) finEval )  \n     }")

;; Recursivos: sumarDigitos y factorial (ejemplos)
(run "sumarDigitos(147)"
     "recursivo (@sumarDigitos(@n)=\n   Si (@n < 10) {\n     @n\n   } sino {\n     ((@n ~ (piso((@n / 10)) * 10)) + evaluar @sumarDigitos(piso((@n / 10))) finEval)\n   }\n ;) { evaluar @sumarDigitos(147) finEval }")

(run "factorial(5)"
     "recursivo (@fact(@n)=\n   Si (@n <= 1) {\n     1\n   } sino {\n     (@n * evaluar @fact((@n ~ 1)) finEval)\n   }\n ;) { evaluar @fact(5) finEval }")

(run "potencia(4,2)"
     "recursivo (@potencia(@base,@exp)=\n   Si (@exp <= 0) {\n     1\n   } sino {\n     (@base * evaluar @potencia(@base,(@exp ~ 1)) finEval)\n   }\n ;) { evaluar @potencia(4,2) finEval }")

(run "sumaRango(2,5)"
     "recursivo (@sumaRango(@a,@b)=\n   Si (@a == @b) {\n     @a\n   } sino {\n     (@a + evaluar @sumaRango((@a + 1),@b) finEval)\n   }\n ;) { evaluar @sumaRango(2,5) finEval }")

;; Decoradores
(run "decorador simple"
     "declarar (\n  @integrantes = procedimiento () { \"Manuela_Steven_Andres\" };\n  @saludar = procedimiento (@proc) {\n    procedimiento () { (\"Hola:\" concat evaluar @proc () finEval) }\n  };\n) {\n  declarar (\n    @decorate = evaluar @saludar (@integrantes) finEval;\n  ) {\n    evaluar @decorate () finEval\n  }\n}")

(run "decorador con mensaje final"
     "declarar (\n  @integrantes = procedimiento () { \"Manuela_Steven_Andres\" };\n  @saludar = procedimiento (@proc) {\n    procedimiento (@mensaje) {\n      ((\"Hola:\" concat evaluar @proc () finEval) concat @mensaje)\n    }\n  };\n) {\n  declarar (\n    @decorate = evaluar @saludar (@integrantes) finEval;\n  ) {\n    evaluar @decorate (\"_ProfesoresFLP\") finEval\n  }\n}")

(displayln "\nPruebas finalizadas.")
