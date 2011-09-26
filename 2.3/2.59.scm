(load "displaylib.scm")
(title "Exercise 2.59")
(print "
       Implement the union-set operation for the unordered-list representation of sets.
       ")

; -- given lib --
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
    set
    (cons x set)))

; -- START --

(define (union-set e f)
  (cond ((null? e) f)
        ((null? f) e)
        ((element-of-set? (car e) f)
            (union-set (cdr e) f))
        (else 
          (cons (car e) (union-set (cdr e) f)))))

(define set1 '(a b c))
(define set2 '(a x y))
(display "(union-set set1 set2)")(newline)
(display (union-set set1 set2))(newline)

