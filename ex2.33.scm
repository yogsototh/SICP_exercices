; Exercise 2.33

(newline) (display "Exercise 2.33") (newline)
; -- Defs --
(define nil ())
(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))


(define (amap p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(display (amap square (list 1 2 3 4))) (newline)

(define (aappend seq1 seq2)
  (accumulate cons seq2 seq1))

(display (aappend (list 1 2 3) (list 4 5 6))) (newline)

(define (alength sequence)
  (accumulate (lambda (x y) (+ y 1)) 0 sequence))

(display (alength (list 1 2 3) )) (newline)
