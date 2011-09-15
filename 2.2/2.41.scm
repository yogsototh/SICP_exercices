(newline)
(display "            ---------------")(newline)
(display "             Exercise 2.41")(newline)
(display "-----------------------------------------------------------")(newline)
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

; Star exercise 2.41
(display "  Write a pro cedure to find all ordered triples")(newline)
(display "  of distinct positive integers i, j and k")(newline)
(display "  less than or equal to a given interger n")(newline)
(display "  that sum to a given integer s")(newline)
(display "-----------------------------------------------------------")(newline)
(newline)
(define (seq start stop)
  (if (> start stop)
    (list)
    (cons start (seq (+ start 1) stop))))

; unique-pairs n -> [ (i,j) | 1<=i<j<=n ]
(define (unique-pairs n)
  (flatmap (lambda (i) 
             (map (lambda (j) (list j i)) 
             (seq 1 (- i 1))))
           (seq 1 n)))

(define (fst l) (car l))
(define (snd l) (cadr l))

(define (is-ordered triplet)
  (and 
    (< (car triplet) (cadr triplet))
    (< (cadr triplet) (caddr triplet))))

(define (unique-triplets n s)
    (map (lambda (doublon)
               (list (car doublon) 
                     (cadr doublon) 
                     (- s 
                        (+ (car doublon) 
                           (cadr doublon)))))
             (unique-pairs n)))

; (display "(unique-triplets 10 10)")(newline)
; (display (unique-triplets 10 10))(newline)

(define (triplets n s)
  (filter 
    is-ordered
    (unique-triplets n s)))

(display "(triplets 10 12)")(newline)
(display (triplets 10 12))(newline)
(newline)
(display "             --- END ---")(newline)
