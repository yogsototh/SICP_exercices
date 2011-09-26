(load "displaylib.scm")
(title "Exercise 2.62")
(print "
        Give a Θ(n) implementation of union-set for sets represented as ordered lists. 
       ")

; -- given lib --
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
    '()    
    (let ((x1 (car set1)) (x2 (car set2)))
      (cond ((= x1 x2)
             (cons x1
                   (intersection-set (cdr set1)
                                     (cdr set2))))
            ((< x1 x2)
             (intersection-set (cdr set1) set2))
            ((< x2 x1)
             (intersection-set set1 (cdr set2)))))))

(define (adjoin-set x set) 
  (cond ((null? set) (list x))
        ((> x (car set)) 
         (cons (car set) (adjoin-set x (cdr set))))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))))
; -- START --

(define (union-set e f)
  (cond ((null? e) f)
        ((null? f) e)
        (else
          (let ((x1 (car e)) (x2 (car f)))
            (cond ((= x1 x2) (cons x1 (union-set (cdr e) (cdr f))))
                  ((< x1 x2) (cons x1 (union-set (cdr e) f)))
                  ((> x1 x2) (cons x2 (union-set e (cdr f)))))))))

(define set1 '(10 20 30))
(define set2 '(20 40 50))
(display "set1 = '")(display set1)(newline)
(display "set2 = '")(display set2)(newline)
(display "(union-set set1 set2)")(newline)
(display (union-set set1 set2))(newline)
