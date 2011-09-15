(newline) (display "Exercise 2.38") (newline)
; -- Defs --
(define nil ())

(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))

(define (fold-left op initial sequence)
    (define (iter result rest)
      (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
    (iter initial sequence))
; -- Start --
; For all initial & l =>
; fold-right op initial l == fold-left op initial l
; <=> (op commutative and associative)
;
; if initial is neutral element of op
; (op x initial) == (op initial x) == x
; Then, it is enough for op to be associative 
; for fold-right & fold-left to return the same value.
