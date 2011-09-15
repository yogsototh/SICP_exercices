(load "displaylib.scm")
(title "Exercise 2.47")
(doc "Here are two possible constructors for frames:

     (define (make-frame origin edge1 edge2)
         (list origin edge1 edge2))

     (define (make-frame origin edge1 edge2)
         (cons origin (cons edge1 edge2)))

For each constructor supply the appropriate selectors to produce an implementation for frames.")

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
(print "List representation")
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))
(define (origin-frame f) (car f))
(define (edge1-frame f) (cadr f))
(define (edge2-frame f) (caddr f))

(define frame (make-frame origin xunit yunit))
(display frame)(newline)
(display (origin-frame frame))(newline)
(display (edge1-frame frame))(newline)
(display (edge2-frame frame))(newline)
(newline)

(print "Cons representation")
(define (make-frame-cons origin edge1 edge2)
  (cons origin (cons edge1 edge2)))
(define (origin-frame-cons f) (car f))
(define (edge1-frame-cons f) (car (cdr f)))
(define (edge2-frame-cons f) (cdr (cdr f)))

(define frame-cons (make-frame-cons origin xunit yunit))
(display frame-cons)(newline)
(display (origin-frame-cons frame-cons))(newline)
(display (edge1-frame-cons frame-cons))(newline)
(display (edge2-frame-cons frame-cons))(newline)
