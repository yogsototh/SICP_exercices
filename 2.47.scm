(load "displaylib.scm")
(title "Exercise 2.46")
(doc "A two-dimensional vector v running from the origin to a point can be represented as a pair consisting of an x-coordinate and a y-coordinate. Implement a data abstraction for vectors by giving a constructor make-vect and corresponding selectors xcor-vect and ycor-vect. In terms of your selectors and constructor, implement procedures add-vect, sub-vect, and scale-vect that perform the operations vector addition, vector subtraction, and multiplying a vector by a scalar:
    
     (x1,y1) + (x2,y2) = (x1+x2 , y1+y2)
     (x1,y1) - (x2,y2) = (x1-x2 , y1-y2)
           s . (x,y)   = (sx, sy)
     ")

(define (make-vect x y) (list x y))
(define (xcor-vect v) (car v))
(define (ycor-vect v) (cadr v))
(define (add-vect v w) (make-vect 
                         (+ (xcor-vect v) (xcor-vect w)) 
                         (+ (ycor-vect v) (ycor-vect w))))
(define (sub-vect v w) (make-vect 
                         (- (xcor-vect v) (xcor-vect w)) 
                         (- (ycor-vect v) (ycor-vect w))))
(define (scale-vect s v) (make-vect 
                            (* s (xcor-vect v)) 
                            (* s (ycor-vect v))))

(define origin (make-vect 0 0))
(define one-one (make-vect 1 1))
(define xunit (make-vect 1 0))
(define yunit (make-vect 0 1))
(define half-half (make-vect 0.5 0.5))

; ------------------------
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))
(define (origin-frame f) (car f))
(define (edge1-frame f) (cadr f))
(define (edge1-frame f) (caddr f))

(print (make-frame origin xunit yunit))


(define (make-frame-cons origin edge1 edge2)
  (cons origin (cons edge1 edge2)))
(define (origin-frame-cons f) (car f))
(define (edge1-frame-cons f) (car (cdr f))
(define (edge1-frame-cons f) (cdr (cdr f))
