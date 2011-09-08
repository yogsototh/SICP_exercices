; Exercise 2.33

(newline) (display "Exercise 2.34") (newline)
; -- Defs --
(define nil ())
(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))
; -- Start --
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ (* higher-terms x) this-coeff))
              0
              coefficient-sequence))

(display (horner-eval 2 (list 1)))
(newline)
(display (horner-eval 2 (list 0 1)))
(newline)
(display (horner-eval 2 (list 1 1)))
(newline)
(display (horner-eval 2 (list 1 3 0 5 0 1)))
(newline)
