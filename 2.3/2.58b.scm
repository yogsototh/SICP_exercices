(load "displaylib.scm")
(title "Exercise 2.58 b")
(print "
       Suppose we want to modify the differentiation program so that it works with ordinary mathematical notation, in which + and * are infix rather than prefix operators. Since the differentiation program is defined in terms of abstract data, we can modify it to work with different representations of expressions solely by changing the predicates, selectors, and constructors that define the representation of the algebraic expressions on which the differentiator is to operate.

       a. Show how to do this in order to differentiate algebraic expressions presented in infix form, such as (x + (3 * (x + (y + 2)))). To simplify the task, assume that + and * always take two arguments and that expressions are fully parenthesized.

       b. The problem becomes substantially harder if we allow standard algebraic notation, such as (x + 3 * (x + y + 2)), which drops unnecessary parentheses and assumes that multiplication is done before addition. Can you design appropriate predicates, selectors, and constructors for this notation such that our derivative program still works? 
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

; ---- END of given library -----

; -- TODO change this
(define (left-of expr symbol)
  (cond ((null? expr) ())
        ((eq? (car expr) symbol) ())
        (else (cons (car expr) (left-of (cdr expr) symbol))))) 

(define (right-of expr symbol)
  (cond ((null? expr) ())
        ((eq? (car expr) symbol) (cdr expr))
        (else (right-of (cdr expr) symbol))))
  
(define (sum? x)
  (and (pair? x) 
       (let ((nb-left (length (left-of x '+)))
             (nb-right (length (right-of x '+))))
         (and (> nb-left 0) 
              (> nb-right 0)
              (odd? nb-left)
              (odd? nb-right)))))

(define (product? x)
  (and (pair? x) 
       (not (sum? x))
       (let ((nb-left (length (left-of x '*)))
             (nb-right (length (right-of x '*))))
         (and (> nb-left 0) 
              (> nb-right 0)
              (odd? nb-left)
              (odd? nb-right)))))


; left of s passed recursively to rewrite in binary format
(define (addend s) (left-expr s '+))
(define (augend s) (right-expr s '+))

(define (multiplier p) (left-expr p '*))
(define (multiplicand p) (right-expr p '*))

(define (simplify e)
  (cond ((= (length e) 1) (car e))
        ((sum? e) (make-sum 
                       (addend e)
                       (augend e)))
        ((product? e) (make-product 
                           (multiplier e) 
                           (multiplicand e)))
        (else (error "unknown expression type -- simplify" e))))


(define (left-expr s op) (simplify (left-of s op)))
(define (right-expr s op) (simplify (right-of s op)))

(define (make-sum a1 a2) 
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list 'a1 + a2))))
(define (make-product m1 m2) 
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list 'm1 * m2))))


; (display "(deriv '(x + 3) 'x)")(newline)
; (display (deriv '(x + 3) 'x))(newline)
; (display "(deriv '(x + (3 * (x + (y + 2)))) 'x)")(newline)
; (display  (deriv '(x + (3 * (x + (y + 2)))) 'x))(newline)
; (display  "(deriv '(x + 3 * (x + y + 2)) 'x)")(newline)
; (display  (deriv '(x + 3 * (x + y + 2)) 'x))(newline)

(define expr1 '(x * 3 + 2))
(define expr2 '(10 * x * x + x * 3 + 2) )
(display  "(deriv ")(display expr1)(display ") 'x)")(newline)
(display  (deriv expr1 'x))(newline)
(display  "(deriv ")(display expr2)(display ") 'x)")(newline)
(display  (deriv expr2 'x))(newline)
