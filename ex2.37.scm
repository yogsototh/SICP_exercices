; Exercise 2.33

(newline) (display "Exercise 2.37") (newline)
; -- Defs --
(define nil ())
(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
    nil
    (cons (accumulate   op init (map car seqs))
          (accumulate-n op init (map cdr seqs)))))

; -- Start --

(define V (list 3 2 1 0))
(define W (list 1 1 1 1))
(define M (list 
            (list 0 1 2 3) 
            (list 1 2 3 4) 
            (list 2 3 4 5)))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v)) m))

(define (transpose mat)
  (accumulate-n (lambda (x acc) (cons x acc)) nil mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (v) (matrix-*-vector cols v)) m)))

(display (dot-product V W)) (newline)
(display (matrix-*-vector M V)) (newline)
(display (transpose M)) (newline)
(display (matrix-*-matrix M (transpose M))) (newline)
