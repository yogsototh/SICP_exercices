;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname factorial.sc) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(define (dragon n)
  (if (< n 3) 
      n
      (+ 
       (dragon (- n 1)) 
       (* 2 (dragon (- n 2))) 
       (* 3 (dragon (- n 3))))))
                                             
(define (fast-dragon n)
  (if (< n 3)
      n
      (dragon-iter 0 1 2 n)))
(define (dragon-iter a b c counter)
  (if (< counter 3)
      c
      (dragon-iter b c (+ c (* 2 b) (* 3 a)) (- counter 1))))

(define (pascal-triangle depth element)
  (cond ((< depth 2) 1)
        ((= 0 element) 1)
        ((= depth element) 1)
        ((> element depth) 0)
        (else (+ (pascal-triangle (- depth 1) element)
                 (pascal-triangle (- depth 1) (- element 1))))))

(define (ylog k x)
  (cond ((< x 0) -1)
        ((<= k 0) -1)
        (else (+ 1 (ylog k (/ x k))))))