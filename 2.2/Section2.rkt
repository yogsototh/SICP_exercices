(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (make-rat n d)
  (cond
    ((= d 0) (error "0 for denominator!"))
    ((and (< n 0) (< d 0)) (make-rat (* n -1) (* d -1)))
    ((< d 0) (make-rat (* n -1) (* d -1)))
    (else 
     (let ((g (gcd n d)))
       (cons (/ n g) (/ d g))))))

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define one-half (make-rat 1 2))
(define one-third (make-rat 1 3))

; Section 2 Exercise 2.2
(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (make-segment begin end) (cons begin end))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

(define (avg x y) (/ (+ x y) 2))

(define (mid-point s)
  (let ((b (start-segment s))
        (e (end-segment s)))
    (let ((xb (x-point b))
          (yb (y-point b))
          (xe (x-point e))
          (ye (y-point e)))
      (make-point (avg xb xe) (avg yb ye)))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define origin (make-point 0 0))
(define x24 (make-point 2 4))
(define test-segment (make-segment origin x24))

; Section 2.2

; Exercise 2.17
(define (last-pairy xs)
  (if (null? (cdr xs)) (car xs) (last-pairy (cdr xs))))

; Exercise 2.18
; Yeah linear time (not quadratic)
(define (yreverse xs)
  (define (reverse-iter l r)
    (if (null? l)
        r
        (reverse-iter (cdr l) (cons (car l) r))))
  (reverse-iter xs (list )))

; Exercise 2.19
(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))
(define (first-denomination coin-value) (car coin-value))
(define (except-first-denomination coin-value) (cdr coin-value))
(define (no-more? coin-values) (null? coin-values))
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

; Exercise 2.20
(define nil ())
(define (same-parity x . xs) 
  (define (f-list t l)
    (if (null? l)
        nil
        (if (t (car l))
            (cons (car l) (f-list t (cdr l)))
            (f-list t (cdr l)))))
  (if (odd? x)
      (f-list odd? (cons x xs))
      (f-list even? (cons x xs))))

; Exercise 2.21
(define (square-list items)
  (if (null? items)
      nil
      (cons (* (car items) (car items)) (square-list (cdr items)))))

(define (square-list-map items)
  (map (lambda (x) (* x x)) items))

; Excercise 2.23
(define (y-for-each f l)
  (cond ((null? l) #t)
      (else (
             (f (car l)) 
             (y-for-each f (cdr l))))))

; -- Section 2.2.2 Hierarchical Structures
(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

; Excercise 2.27
(define (deep-reverse xs)
  (define (reverse-iter l r)
    (if (null? l)
        r
        (reverse-iter (cdr l) (cons (deep-reverse (car l)) r))))
  (if (not (pair? xs)) 
      xs ; leaf case
      (reverse-iter xs nil)))

; Exercise 2.28
(define (fringe xs)
  (if (null? xs)
      nil
      (if (not (pair? xs))
          (list xs)
          (append (fringe (car xs)) (fringe (cdr xs))))))

; (define x (list (list 1 2) (list 3 4)))
; (define (deeplist n)
;   (if (= n 0)
;       x
;       (list (deeplist (- n 1)) (deeplist (- n 1)))))

; Exercise 2.29
(define (make-mobile left right)
  (list left right))
(define (make-branch length structure)
  (list length structure))
; a
(define (left-branch m) (car m))
(define (right-branch m) (car (cdr m)))
(define (branch-length b) (car b))
(define (branch-structure b) (car (cdr b)))
; needed for b and c
(define (is-weight? m) (not (pair? m)))
; b

; define exterior to total-weight for question c
(define (total-br-weight br)
  (let ((str (branch-structure br)))
    (if (is-weight? str)
        str
        (total-weight str))) )

(define (total-weight m)
  (if (null? m)
      nil
      (+ (total-br-weight (left-branch m)) 
         (total-br-weight (right-branch m)))))

(define m (make-mobile 
           (make-branch 3 5) 
           (make-branch 1 
                        (make-mobile 
                         (make-branch 2 5) 
                         (make-branch 1 10)))))

; c

(define (balanced m)
  (if (is-weight? m)
      #t
      (let ((left-length (branch-length (left-branch m)))
            (left-weight (total-br-weight (left-branch m)))
            (right-length (branch-length (right-branch m)))
            (right-weight (total-br-weight (right-branch m)))
            (left-mobile (branch-structure (left-branch m)))
            (right-mobile (branch-structure (right-branch m)))
            )
        (and 
         (= (* left-length left-weight)
            (* right-length right-weight))
         (balanced left-mobile)
         (balanced right-mobile)))))

; -- Mapping over trees

; Exercise 2.30

(define (square-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (* tree tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

(define (msquare-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (msquare-tree sub-tree)
             (* sub-tree sub-tree))) 
       tree))

(define t (list 1
                (list 2 (list 3 4) 5)
                (list 6 7)))
                        
; Exercise 2.31

(define (tree-map f tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map f sub-tree)
             (f sub-tree))) 
       tree))

(define (square x) (* x x))
(define (square-tree2 tree) (tree-map square tree))

; Exercise 2.32 ??? don't understand why...
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (sub) (cons (car s) sub)) rest)))))
         
; -- 2.2.3 Sequences as conventional interfaces
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

; Exercise 2.33
(define (acc_map f sequence)
  (accumulate (lambda (x y) (cons (f x) y)) nil sequence))
(define (acc_append seq1 seq2)
  (accumulate cons seq2 seq1))
(define (acc_length sequence)
  (accumulate (lambda (x acc) (+ acc 1)) 0 sequence))

; Exercise 2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ (* x higher-terms) this-coeff))
              0
              coefficient-sequence))
