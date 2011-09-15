(newline) (display "Exercise 2.39") (newline)
; -- Defs --
(define nil ())
; -- Start --
(define (reverser sequence)
  (fold-right (lambda (x y) (append y (list x))) nil sequence))
(define (reversel sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))
(newline) (display (reverser (list 2 3 9))) (newline)
(newline) (display (reversel (list 2 3 9))) (newline)
