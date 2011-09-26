(load "displaylib.scm")
(title "Exercise 2.60")
(print "
       We specified that a set would be represented as a list with no duplicates. Now suppose we allow duplicates. For instance, the set {1,2,3} could be represented as the list (2 3 2 1 3 2 2). Design procedures element-of-set?, adjoin-set, union-set, and intersection-set that operate on this representation. How does the efficiency of each compare with the corresponding procedure for the non-duplicate representation? Are there applications for which you would use this representation in preference to the non-duplicate one? 
       ")

; -- given lib --
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)        
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define (adjoin-set x set) (cons x set))

; -- START --

(define (union-set e f) (append e f))

(define set1 '(a b c))
(define set2 '(a x y))
(display "set1 = '")(display set1)(newline)
(display "set2 = '")(display set2)(newline)
(display "(union-set set1 set2)")(newline)
(display (union-set set1 set2))(newline)
(display "(intersection-set set1 set2)")(newline)
(display (intersection-set set1 set2))(newline)
(display "(adjoin-set 'a set1)")(newline)
(display (adjoin-set 'a set1))(newline)

; Worse if you have to manage a lot of duplicate values.
; Better if you have to manage very few duplicate values (sparse set).

