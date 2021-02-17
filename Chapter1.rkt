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
(define (ackermann x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (ackermann (- x 1) (ackermann x (- y 1))))))
(ackermann 1 10)
(ackermann 2 4)
(ackermann 3 3)
; 1.2.2 Tree Recursion
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))
(define (fibiter n)
  (fibstep 1 0 n))
(define (fibstep a b count)
  (if (= count 0)
      b ; a = a + b, b = a
      (fibstep (+ a b) a (- count 1))))

; to get the 5th number we get:
; (+ a b) = a_n, a = b_n)
; (+ a_n b_n) = a_n+1, a_n = b_n+1
; => linear iteration, only order of N steps required to execute function
; But the interpreter itself uses tree recursion to operate on hierarchical data
; Exercise 1.11
; recursive procedure
(define (fun n)
  (if (< n 3)
      n
      (+ (fun (- n 1)) (* 2 (fun (- n 2))) (* 3 (fun (- n 3))))))
; iterative procedure
(define (fun2 n)
  (if (< n 3)
      n
      (fiter 2 1 0 n)))

(define (fiter a b c count) ; the number n becomes our count => capture state
  (if (< count 3)
      a
      (fiter (+ a (* 2 b) (* 3 c))
             a ; a becomes new b
             b ; b becomes new c
             (- count 1))))

(define (pascal row col)
  (cond ((< row 3) 1)
        ((= col 1) 1)
        ((= col row) 1) ; need to check for rightmost column
        (else (+ (pascal (- row 1) (- col 1)) (pascal (- row 1) col)))))

; 1.2.4 Exponentiation
; a to the power of b is equal to a times a times a ... times a; b times
(define (expt a b)
  (cond ((= b 0) 1)
        ((= b 1) a)
        (else (* a (expt a (- b 1))))))

; iterative exponent
(define (expt2 b n)
  (expt-iter b n 1))

(define (expt-iter b counter product)
  (if (= counter 0)
      product
      (expt-iter b (- counter 1) (* b product)))) ; we carry the product with us
; this linear recursion is much faster.
; even faster exponentiation with recursive squaring
(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (even? n)
  (= (remainder n 2) 0))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

; need remainder operation 
(define (remainder a b)
  (cond ((= 0 (- a b)) 0)
        ((< (- a b) 0) a)
        (else (remainder (- a b) b))))

; searching for divisors
(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))
(define (divides? a b)
  (= 0 (remainder b a)))

; 1.23
(define (next input)
  (if (= input 2)
      3
      (+ input 2)))
; this lets us write a really short primality test
(define (prime? n)
  (= n (smallest-divisor n)))

; 1.21
(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)

; 1.22
(define (timed-prime-test n)
  (newline); newline keyword
  (display n); display keyword 
  (start-prime-test n (runtime))); runtime keyword

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display "***")
  (display elapsed-time))
; procedure that searches for primes in given range
(define (search-for-primes start end)
  (timed-prime-test start)
  (cond ((= start end)
         (newline)
         (display "Search Finished."))
        (else (search-for-primes (+ start 1) end))))

; 1.3 Formulating abstractions with higher order procedures
(define (sum-integers a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))

(define (sum-cubes a b)
  (if (> a b)
      0
      (+ (cube a) (sum-cubes (+ a 1) b))))
; If we want to define a procedure that returns the sum of numbers in a range
; combined with an arbitrary procedure we need quoting (I think)
(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2)))
         (pi-sum (+ a 4) b))))

(define (sum fun a next b)
  (if (> a b)
      0
      (+ (fun a)
         (sum fun (next a) next b))))

(define (sum-cubes2 a b)
  (sum cube a inc b))
; we dont even have to quote anything yet, we can just pass text that gets
; substituted automatically.
(define (identity x) x)
(define (sum-integers2 a b)
  (sum identity a inc b))

(define (pifun a)
  (/ 1.0 (* a (+ a 2))))
(define (pinext a)
  (+ a 4))
(define (pi-sum2 a b)
  (sum pifun a pinext b))

; we can use this as a building block in numerical integration
(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* dx (sum f (+ a (/ dx 2.0)) add-dx b)))

; 1.29 Simpson's Rule is a more accurate method of numerical integration
; h = (b - a)/n
; yk = f(a + kh)
; integral = (h/3) * (y0 + 4y1 + 2y2 + 4y3 + ... + 4yn-1 + yn)

(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (simp-next x) (+ x (* 2.0 h)))
  (* (/ h 3.0)
     (+ (f a) (f b)
        (* 4.0 (sum f (+ a h) simp-next b))
        (* 2.0 (sum f (+ a (* 2.0 h)) simp-next (- b h))))))

; 1.30