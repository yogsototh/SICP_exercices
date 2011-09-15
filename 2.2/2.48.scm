(load "displaylib.scm")
(title "Exercise 2.48")
(doc "A directed line segment in the plane can be represented as a pair of vectors -- the vector running from the origin to the start-point of the segment, and the vector running from the origin to the end-point of the segment. Use your vector representation from exercise 2.46 to define a representation for segments with a constructor make-segment and selectors start-segment and end-segment.") 

; --- Vectors 2.46 ---
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

; -- Frames 2.47 --
; --- frame with list representation ---
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))
(define (origin-frame f) (car f))
(define (edge1-frame f) (cadr f))
(define (edge2-frame f) (caddr f))
; --- frame with cons representation ---
(define (make-frame-cons origin edge1 edge2)(cons origin (cons edge1 edge2)))
(define (origin-frame-cons f) (car f))
(define (edge1-frame-cons f) (car (cdr f)))
(define (edge2-frame-cons f) (cdr (cdr f)))
; ---------------------
;       START
; ---------------------

(define (make-segment start end) (list start end))
(define (start-segment s) (car s))
(define (end-segment s) (cadr s))

(print "START")
(define segment (make-segment half-half one-one))
(display segment)(newline) 
(display (start-segment segment))(newline) 
(display (end-segment segment))(newline) 
