(load "displaylib.scm")
(title "Exercise 2.64")
(print "
       Use the results of exercises 2.63 and  2.64 to give (n) implementations of union-set and intersection-set for sets implemented as (balanced) binary trees.
       ")

; The implementation of union-set and intersection set in ϴ(n) can be done using
; an intermediate ordered list representation.
; We have a ϴ(n) implementation for union and intersection for ordered list
; We have a ϴ(n) for tree -> ordered list
; We have a ϴ(n) for list -> balanced tree


; -- transform a tree into an ordered list
(define (tree->list tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
      result-list
      (copy-to-list (left-branch tree)
                    (cons (entry tree)
                          (copy-to-list (right-branch tree)
                                        result-list)))))
  (copy-to-list tree '()))


; Transform an ordered list into a balanced tree
(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
    (cons '() elts)
    (let ((left-size (quotient (- n 1) 2)))
      (let ((left-result (partial-tree elts left-size)))
        (let ((left-tree (car left-result))
              (non-left-elts (cdr left-result))
              (right-size (- n (+ left-size 1))))
          (let ((this-entry (car non-left-elts))
                (right-result (partial-tree (cdr non-left-elts)
                                            right-size)))
            (let ((right-tree (car right-result))
                  (remaining-elts (cdr right-result)))
              (cons (make-tree this-entry left-tree right-tree)
                    remaining-elts))))))))

; ϴ(n) union for two ordered list
(define (ordlist-union-set e f)
  (cond ((null? e) f)
        ((null? f) e)
        (else
          (let ((x1 (car e)) (x2 (car f)))
            (cond ((= x1 x2) (cons x1 (ordlist-union-set (cdr e) (cdr f))))
                  ((< x1 x2) (cons x1 (ordlist-union-set (cdr e) f)))
                  ((> x1 x2) (cons x2 (ordlist-union-set e (cdr f)))))))))

; ϴ(n) intersection for two ordered list
(define (ordlist-intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
    '()    
    (let ((x1 (car set1)) (x2 (car set2)))
      (cond ((= x1 x2)
             (cons x1
                   (ordlist-intersection-set (cdr set1)
                                     (cdr set2))))
            ((< x1 x2)
             (ordlist-intersection-set (cdr set1) set2))
            ((< x2 x1)
             (ordlist-intersection-set set1 (cdr set2)))))))

(define (union-set set1 set2)
  (let ((ordlist-set1 (tree->list set1))
        (ordlist-set2 (tree->list set2)))
    (list->tree (ordlist-union-set ordlist-set1 ordlist-set2))))

(define (intersection-set set1 set2)
  (let ((ordlist-set1 (tree->list set1))
        (ordlist-set2 (tree->list set2)))
    (list->tree (ordlist-intersection-set ordlist-set1 ordlist-set2))))
