(load "displaylib.scm")
(title "Exercise 2.56")
(print "
       Show how to extend the basic differentiator to handle more kinds of expressions. For instance, implement the differentiation rule

 d(u^n)             du
 ------ = nu^{n-1} ----
   dx               dx

       by adding a new clause to the deriv program and defining appropriate procedures exponentiation?, base, exponent, and make-exponentiation. (You may use the symbol ** to denote exponentiation.) Build in the rules that anything raised to the power 0 is 1 and anything raised to the power 1 is the thing itself. 
       ")

; -- Given library

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

; Naive
; (define (make-sum a1 a2) (list '+ a1 a2))
; (define (make-product m1 m2) (list '* m1 m2))

(define (=number? x v)
  (and (number? x) (= x v)))
(define (make-sum a1 a2) 
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))
(define (make-product m1 m2) 
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))

; ---- END of given library -----


; Added exponentation case
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        ((exponentiation? exp)
         (let ((u (base exp))
               (n (exponent exp)))
           (make-product
             (make-product n
               (make-exponentiation u (make-sum n -1)))
             (deriv u var))))
        (else
          (error "unknown expression type -- DERIV" exp))))

(define (exponentiation? x) (and (pair? x) (eq? (car x) '**)))
(define (base e) (cadr e))
(define (exponent e) (caddr e))
(define (make-exponentiation u n)
  (cond ((=number? n 0) 1)
        ((=number? n 1) u)
        ((and (number? u) (number? n)) (pow u n))
        (else (list '** u n))))

(display "(deriv '(+ x 3) 'x)")(newline)
(display (deriv '(+ x 3) 'x))(newline)
(display "(deriv '(* x y) 'x)")(newline)
(display (deriv '(* x y) 'x))(newline)
(display "(deriv '(* (* x y) (+ x 3)) 'x)")(newline)
(display (deriv '(* (* x y) (+ x 3)) 'x))(newline)
(display "(deriv '(** x y) 'x)")(newline)
(display (deriv '(** x y) 'x))(newline)
(display "(deriv '(** x 2) 'x)")(newline)
(display (deriv '(** x 2) 'x))(newline)
(display "(deriv '(** x (** x 2)) 'x)")(newline)
(display (deriv '(** x (** x 2)) 'x))(newline)
