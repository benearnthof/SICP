#lang sicp
; Elements of Programming: Combining primitive expressions to keep abstracting.
; Lisp primitives:
(+ 1 4)
(* 34 2)
; We can chain operations implicitly
(+ 1 2 3 4 5 6 7 8 9)
(* 2 2 2 2 2 2 2 2 2)
; => Prefix Notation
; This allows arbitrarily deep nesting
(+ (* 3
      (+ (* 2 4)
         (+ 3 5)))
   (+ (- 10 7)
      6))
; We evaluate expressions "from the inside out"
; We name things with the define keyword => Variables, functions etc.
(define size 2)
(* size 100)
; further examples
(define pi 3.14159)
(define radius 10)
(* pi (* radius radius))
(define circumference (* 2 pi radius))
; how do we write functions? => extra parenthesis
(define (square x)
  (* x x))
; general form of a procedure (function) definition is:
; (define (<name> <formals>) (body))
; pretty R like
; the nested syntax allows us to easily chain procedures like in maths
(define (sumofsquares x y)
  (+ (square x) (square y)))
; that's pretty neat!
; we can also alias functions
(define (f a)
  (sumofsquares (+ a 1)(* a 2)))
; this follows the substitution model of evaluation
; the lisp interpreter uses applicative order evaluation. This differs slightly
; from normal order application => Chapter 3 and 4

; Conditional Expressions and Predicates => Expression that is TRUE or FALSE
(define (abs_long x)
  (cond ((> x 0) x); If x is larger than 0 return x
   ((= x 0) 0); no double cond needed since we can just implicitly extend
  ((< x 0) (- x))))
; The general form of a conditional expression is
; (cond (<p1><e1>)
;       (<p2><e2>)
;        ...
;       (<pn><en>)
; The pairs of expressions in the parenthesis are called clauses
; p is a predicate => exp that is interpreted as either TRUE or FALSE

; we can of course redefine the abs function in a more compact way
(define (abs x)
  (cond ((< x 0) (- x))
        (else x)))
; if x is less than 0 return negative x else return x
; cond feels like a switch statement in other languages, the if keyword
; allows even compacter clauses
(define (abs2 x)
  (if (< x 0)
      (- x)
      x))
; if => then => else
; (if ⟨predicate⟩ ⟨consequent⟩ ⟨alternative⟩)

; To evaluate an if expression, the interpreter starts by evaluating the pred.
; primitive predicates: < = > and or not
(define x 8)
(and (> x 5)(< x 10))
; we can combine these predicates to get functions such as >=
; => we can directly use these symbols as function names
(define (<= x y)
  (not (> x y)))
; TODO: Exercises
; TODO: Fix stream setup => Audio issues.
; TODO: Add mic and webcam
; TODO: Learn DrRacket hotkeys