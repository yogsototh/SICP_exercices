(load "displaylib.scm")
(title "Exercise 2.61")
(print "
       Give an implementation of adjoin-set using the ordered representation. By analogy with element-of-set? show how to take advantage of the ordering to produce a procedure that requires on the average about half as many steps as with the unordered representation. 
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

; -- START --
(define (adjoin-set x set) 
  (cond ((null? set) (list x))
        ((> x (car set)) 
         (cons (car set) (adjoin-set x (cdr set))))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))))

(define set1 '(10 20 30))
(define set2 '(20 40 50))
(display "set1 = '")(display set1)(newline)
(display "set2 = '")(display set2)(newline)
(display "(intersection-set set1 set2)")(newline)
(display (intersection-set set1 set2))(newline)
(display "(adjoin-set 30 set1)")(newline)
(display (adjoin-set 30 set1))(newline)
(display "(adjoin-set 25 set1)")(newline)
(display (adjoin-set 25 set1))(newline)
