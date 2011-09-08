(define nil ())
(define (square x) (* x x))

; Exercise 2.31

(define (tree-map f t)
  (cond ((null? t) nil)
        ((not (pair? t)) (f t))
        (else (cons (tree-map f (car t))
                    (tree-map f (cdr t))))))

(define x (list 1
            (list 2 (list 3 4) 5)
            (list 6 7)))

(define (tree-square t)
  (tree-map square t))

; -- Print Results --
(newline)
(display "Exercise 2.31 (tree-map)")
(newline)
(display x)
(newline)
(display (tree-square x))

; Exercise 2.32
(newline)
(display "Exercise 2.32 (subsets)")

(define (subsets s)
  (if (null? s)
    (list nil)
    (let ((rest (subsets (cdr s))))
      (append rest (map (lambda (x) (list x (cons (car s) x))  rest))))))

(subsets (list 1 2 3))

