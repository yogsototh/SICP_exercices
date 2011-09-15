(load "displaylib.scm")
(load "imagelib.scm")
(title "Exercise 2.50")
(doc "Define the transformation flip-horiz, which flips painters horizontally, and transformations that rotate painters counterclockwise by 180 degrees and 270 degrees.") 
; Exercise start

; -- define transform-painter --
(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
          (make-frame new-origin
                      (sub-vect (m corner1) new-origin)
                      (sub-vect (m corner2) new-origin)))))))


(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)   ; new origin
                     (make-vect 0.0 0.0)   ; new end of edge1
                     (make-vect 1.0 1.0))) ; new end of edge2

(define (rot-180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)   
                     (make-vect 0.0 1.0)   
                     (make-vect 1.0 0.0))) 

(define (rot-270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)   
                     (make-vect 0.0 0.0)   
                     (make-vect 1.0 1.0))) 

; I won't do the wave painter, I have no data
(define frame (make-frame origin (scale-vect 2 xunit) yunit))
(print "-- X frame --")
(draw-X frame)
(print "-- flip-horz frame --")
((flip-horiz draw-X) frame)
(print "-- rot-180 frame --")
((rot-180 draw-X) frame)
(print "-- rot-270 frame --")
((rot-270 draw-X) frame)
