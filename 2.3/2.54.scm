(load "displaylib.scm")
(title "Exercise 2.54")
(print "Two lists are said to be equal? if they contain equal elements arranged in the same order. For example,

       (equal? '(this is a list) '(this is a list))

       is true, but

       (equal? '(this is a list) '(this (is a) list))

       is false. To be more precise, we can define equal? recursively in terms of the basic eq? equality of symbols by saying that a and b are equal? if they are both symbols and the symbols are eq?, or if they are both lists such that (car a) is equal? to (car b) and (cdr a) is equal? to (cdr b). Using this idea, implement equal? as a procedure.

")

(define (my-equal? l1 l2)
  (if (and (symbol? l1) (symbol? l2))
    (eq? l1 l2)
    (if (and (list? l1) (list? l2))
      (or
        (and (null? l1) (null? l2))
        (and
          (eq? (car l1) (car l2))
          (my-equal? (cdr l1) (cdr l2))))
      #f
      )))

(define example '(this is a list))
(define example2 '(this (is a) list))
(display "(my-equal? example example)")(newline)
(display (my-equal? example example))(newline)
(display "(my-equal? example example2)")(newline)
(display (my-equal? example example2))(newline)
