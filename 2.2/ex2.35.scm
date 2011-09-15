; Exercise 2.33

(newline) (display "Exercise 2.35") (newline)
; -- Defs --
(define nil ())
(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))
; -- Start --
(define (count-leaves t)
  (accumulate + 0 (map 
                    (lambda (x) 
                      (if (not (pair? x)) 
                        1 
                        (count-leaves x))) t)))

(define tree (list 1 (list 2 3) (list 4 (list 5))))
(display tree)(newline)
(display (count-leaves tree)) (newline)
