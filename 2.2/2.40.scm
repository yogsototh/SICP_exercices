(newline)
(display "Exercise 2.40")(newline)
; -- Defs --
(define nil ())
(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))
; -- Start --
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))
; --------

(define (permutations s)
  (if (null? s)                    ; empty set?
    (list nil)                   ; sequence containing empty set
    (flatmap (lambda (x)
               (map (lambda (p) (cons x p))
                    (permutations (remove x s))))
             s)))
(define (remove item sequence) 
  (filter (lambda (x) (not (= item x))) 
          sequence))

; (display "Permutations")(newline)
; (display (permutations (list 1 2 3)))(newline)

; Star exercise 2.40
; Define a procedure unique-pairs that, given an integer n, generates the sequence of pairs (i,j) with (1 <= j < i <= n). Use unique-pairs to simplify the definition of prime-sum-pairs
(define (seq start stop)
  (if (> start stop)
    (list)
    (cons start (seq (+ start 1) stop))))

; (display "(seq 1 3)")(newline)
; (display (seq 1 3))(newline)

; unique-pairs n -> [ (i,j) | 1<=i<j<=n ]
(define (unique-pairs n)
  (flatmap (lambda (i) 
             (map (lambda (j) (list j i)) 
             (seq 1 (- i 1))))
           (seq 1 n)))


; simplified version of prime-sum

; true if m | n
; false otherwise
(define (div? m n)
  (= (modulo n m) 0))

; true if n is prime
(define (prime? n) 
  (accumulate 
        (lambda (x y) 
           (and y (not (div? x n)) ))
         true 
         (seq 2 (floor (sqrt n)))))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
    (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))
(define (prime-sum-pairs n)
  (map make-pair-sum
         (filter prime-sum? (unique-pairs n))))

; (display "(unique-pairs 10)")(newline)
; (display (unique-pairs 10))(newline)
; (display "(filter prime-sum? (unique-pairs 10))")(newline)
; (display (filter prime-sum? (unique-pairs 10)))(newline)
; (display "(make-pair-sum (list 3 4))")(newline)
; (display (make-pair-sum (list 3 4)))(newline)

(display "(prime-sum-pairs 10)")(newline)
(display (prime-sum-pairs 10))(newline)
