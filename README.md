# Taller 3 — Fundamentos de Lenguajes de Programación

**Universidad del Valle · Escuela de Ingeniería de Sistemas y Computación**  
Semestre 2026-I

---

## Integrantes

| Nombre | Código |
|---|---|
| Manuela Martínez Moncada | 2375458 |
| Steven Fernando Aragón Álvarez | 2418804 |
| Andrés Gerardo González Rosero | 2416541 |

---

## Descripción

Implementación de un intérprete para un lenguaje de programación con notación infija, desarrollado en **Racket/EOPL**. El intérprete incluye análisis léxico, análisis sintáctico (parser), construcción del AST y evaluación sobre un ambiente encadenado.

---

## Características del lenguaje

| Característica | Sintaxis |
|---|---|
| Números (enteros y decimales, positivos y negativos) | `42`, `-3`, `3.14`, `-0.5` |
| Textos | `"hola"`, `"FLP"` |
| Variables | `@x`, `@nombre` (inician con `@`) |
| Resta (notación infija) | `(4 ~ 5)` → `-1` |
| Operaciones binarias | `(exp + exp)`, `(exp * exp)`, `(exp concat exp)`, etc. |
| Operaciones unarias | `longitud(exp)`, `add1(exp)`, `neg(exp)`, `piso(exp)` |
| Condicional | `Si exp { exp } sino { exp }` |
| Variables locales | `declarar (@x=1; @y=2;) { exp }` |
| Procedimientos | `procedimiento (@x, @y) { exp }` |
| Aplicación | `evaluar @f (arg1, arg2) finEval` |
| Recursión | `recursivo (@f(@n) = exp ;) { exp }` |
| Comentarios | `// comentario de línea` |

---

## Ambiente inicial

El intérprete arranca con las siguientes variables predefinidas:

```bash
@a → 1
@b → 2
@c → 3
@d → "hola"
@e → "FLP"
```

---

## Uso

Abre `interpretador.rkt` en DrRacket y usa la función `interprete` o su alias `interpretador`:

```racket
(interprete "@a")
; → 1

(interprete "(4 ~ -5)")
; → 9

(interprete "Si (5 > 9) { 1 } sino { 0 }")
; → 0

(interprete "declarar (@x=2; @y=3;) { (@x + @y) }")
; → 5
```

## Pruebas automáticas

Se incluye una suite de pruebas que ejecuta los ejemplos del enunciado y verifica resultados.
Ejecutar desde la raíz del proyecto:

```sh
racket test.rkt
```

Los resultados y errores se muestran por consola.

## Notas sobre la implementación actual

- Token `texto`: las cadenas deben aparecer entre comillas y pueden iniciar con letra o con `_`.
- Dentro del texto se permiten letras, dígitos, `_` y `:` (esto permite literales como "Hola:").

---

## Ejercicios del punto 9

Los programas escritos en el lenguaje están al final del archivo `interpretador.rkt`, comentados y numerados.

### 9a — Suma de dígitos

```scheme
evaluar @sumarDigitos(147) finEval  →  12
```

### 9b — Factorial

```scheme
evaluar @fact(5)  finEval  →  120
evaluar @fact(10) finEval  →  3628800
```

### 9c — Potencia recursiva

```scheme
evaluar @potencia(4, 2) finEval  →  16
```

### 9d — Suma de rango

```scheme
evaluar @sumaRango(2, 5) finEval  →  14
```

### 9e — Decorador simple

```scheme
evaluar @decorate() finEval  →  "Hola:Manuela_Steven_Andres"
```

### 9f — Decorador con mensaje final

```scheme
evaluar @decorate("_ProfesoresFLP") finEval  →  "Hola:Manuela_Steven_Andres_ProfesoresFLP"
```

---

## Estructura del archivo

```scheme
interpretador.rkt
├── Especificación léxica       (scanner-spec)
├── Gramática                   (grammar)
├── Datatypes automáticos       (sllgen)
├── Parser                      (scan&parse)
├── Datatype de ambiente        (ambiente)
├── Funciones de ambiente       (buscar-variable, extender-ambiente, ...)
├── Ambiente inicial            (ambiente-init)
├── Datatype procVal            (cerradura)
├── Evaluador                   (evaluar-expresion)
├── Aplicar cerradura           (aplicar-cerradura)
├── Ambiente recursivo          (extender-ambiente-recursivo)
├── Primitivas binarias         (aplicar-primitiva-bin)
├── Primitivas unarias          (aplicar-primitiva-un)
├── Intérprete                  (interprete / interpretador)
└── Ejercicios 9a–9f            (comentados al final)
```

---

## Decisiones de diseño

- **Resta con `~`:** se usa la virgulilla en lugar de `-` para evitar ambigüedad con números negativos en notación infija.
- **Booleanos:** representados numéricamente — `0` es falso, cualquier otro valor es verdadero.
- **Recursión mutua:** implementada con un vector compartido (`amb-ref`) que permite que las cerraduras se vean entre sí antes de que el ambiente esté completamente construido.
- **Identificadores:** todos inician con `@` para distinguirlos de palabras reservadas del lenguaje.
