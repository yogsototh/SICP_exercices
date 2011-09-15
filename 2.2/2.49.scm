(load "displaylib.scm")
(load "imagelib.scm")
(title "Exercise 2.49")
(doc "Use segments->painter to define the following primitive painters:
    a.  The painter that draws the outline of the designated frame.
    b.  The painter that draws an ``X'' by connecting opposite corners of the frame.
    c.  The painter that draws a diamond shape by connecting the midpoints of the sides of the frame.
    d.  The wave painter.") 
; Exercise start

(define (outline-frame f)
  ((segments->painter 
    (list (make-segment origin yunit)
          (make-segment yunit one-one)
          (make-segment one-one xunit)
          (make-segment xunit origin))) f))

(define (draw-X f) 
  ((segments->painter 
     (list (make-segment origin one-one)
           (make-segment xunit yunit))) f))

(define (mid-point A B) (scale-vect .5 (add-vect A B)))

(define (diamond f)
 ((segments->painter 
          (list 
            (make-segment (make-vect .5 0) (make-vect 1 .5))
            (make-segment (make-vect 1 .5) (make-vect .5 1))
            (make-segment (make-vect .5 1) (make-vect 0 .5))
            (make-segment (make-vect 0 .5) (make-vect .5 0)))) f))

; I won't do the wave painter, I have no data
(define frame (make-frame origin (scale-vect 2 xunit) yunit))
(print "-- draw-line test --")
(draw-line origin one-one)
(print "-- segments->painter test --")
(segments->painter (list (make-vect origin one-one) (make-vect half-half one-one)))
(print "-- Outline frame --")
(outline-frame frame)
(print "-- X frame --")
(draw-X frame)
(print "-- diamond --")
(diamond frame)
