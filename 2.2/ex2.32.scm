; Exercise 2.32
(define nil ())
(newline)
(display "Exercise 2.32 (subsets)")

(define (subsets s)
  (if (null? s)
    (list nil)
    (let ((rest (subsets (cdr s)))
          (x (car s)))
      (append rest (map (lambda (r) (cons x r))  rest)))))

(define l (list 1 2 3))
(newline)
(display "Subsets of ")
(display l)
(newline)
(display (subsets l))
