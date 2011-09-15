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
(display origin)(newline)

(define one-one (make-vect 1 1))
(define half-half (make-vect 0.5 0.5))

(print "(add-vect one-one half-half)")
(display (add-vect one-one half-half))(newline)
(print "(sub-vect one-one half-half)")
(display (sub-vect one-one half-half))(newline)

(display "(scale-vect .75 one-one)")(newline)
(display (scale-vect .75 one-one))(newline)


