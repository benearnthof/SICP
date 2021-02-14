#lang sicp
; TODO: Learn DrRacket hotkeys
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
(+ 5 3 4)
(- 9 1)
(/ 6 2)
(+ (* 2 4)(- 4 6))
(define a 3)
(define b (+ a 1))
(+ a b (* a b))
(= a b)
(if (and (> b a) (< (* a b)))
    b
    a)
; returns b
(+ 2 (if (> b a) b a)) ; returns 6
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25)) ; should return 16
(+ 2 (if (> b a) b a)) ; should return 6

(* (cond ((> a b) a)
   ((< a b) b)
   (else -1))
( + a 1)) ; 4 * 4 evaluates to 16
; 1.2 Translate into prefix form:
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7))) ; seems correct
; 1.3 Define a procedure that takes three numbers as arguments and returns the
; sum of the squares of the two larger numbers
(define (ssqlarge x y z)
  (cond ((and (< x y) (< x z)) (sumofsquares z y))
        ((and (< y x) (< y z)) (sumofsquares x z))
        ((and (< z y) (< z x)) (sumofsquares x y))))

; 1.4 Observe that our model of evaluation allows for combinations whose
; operators are compound expressions. Use this observation to describe the
; behavior of the following procedure
(define (a_plus_abs_b a b)
  ((if (> b 0) + -) a b))
; the operator to combine a and b is itself a compound expression
; this is really slick, im not gonna lie

; 1.5 Applicative vs normal order evaluation
;(define (p) (p))
(define (test x y)
  (if (= x 0) 0 y))
; applicative order evaluation will unnest first, thus the test in the book
; should just crash since evaluating p just returns p in an endless loop
; normal order evaluation should just return 0
;(test 0 (p))

; Newtons method for square roots
(define (sqrtiter guess x)
  (if (goodenough? guess x)
      guess
      (sqrtiter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (goodenough? guess x)
  (< (abs (- (square guess) x)) 0.000001))

; recursion allows us to avoid loops
(define (sqrt x)
  (sqrtiter 1 x))

; 1.7
(define (sqrt2 x)
  (doiter 1 x))

(define (doiter guess x)
  (if (< (abs (- guess (improve guess x))) 0.000001)
      guess
      (doiter (improve guess x) x)))

; 1.8 Newton's method for cube roots is based on the fact that if y is an
; approximation to the cube root of x then a better approximation is given by
; ((x/y^2) + (2y))/3
(define (cuberoot x)
  (cuberootiter 1 x))

(define (cuberootiter guess x)
  (if (goodenoughcube? guess x)
      guess
      (cuberootiter (improvecube guess x) x)))

(define (improvecube y x)
  (/ (+ (/ x (square y))(* 2 y)) 3))

(define (goodenoughcube? guess x)
  (< (abs (- (cube guess) x)) 0.000001))

(define (cube x)
  (* x x x))

; one detail of a procedure's implementation that should not matter to the user
; of the procedure is the implementer's choice of names
; => Variable scoping
; To allow other functions that have names like iter etc. We can rewrite the
; sqrt procedure as follows:
(define (blocksqrt x)
  (define (goodenough? guess x)
    (< (abs (- (square guess) x)) 0.000001))
  (define (improve guess x) (average guess (/ x guess)))
  (define (sqrtiter guess x)
    (if (goodenough? guess x)
        guess
        (sqrtiter (improve guess x) x)))
  (sqrtiter 1.0 x))
; Such nesting of definitions is called block structure
; we can clean this code up by using lexical scoping
(define (finalsqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))
; because x is a function variable on the outermost layer it is implicitly
; available for all inner definitions

; 1.2 Procedures and the Processes They Generate
; Linear Recursion and Iteration
(define (factorial n)
  (if (= n 1) 1
      (* n (factorial (- n 1)))))

(define (factorial2 n)
  (fact-iter 1 1 n))

(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))

; Every iterative process can be realized "in hardware" as a machine that has
; a fixed set of registers and no auxiliary memory.
; In contrast, recursive processes require a machine that uses an auxiliary
; data structure known as a stack.
; Usually any linearly recursive function can be captured with state variables
; => Tail recursive
; Exercise 1.9
