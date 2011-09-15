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

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
            (transform-painter painter1
                               (make-vect 0.0 0.0)
                               split-point
                               (make-vect 0.0 1.0)))
          (paint-right
            (transform-painter painter2
                               split-point
                               (make-vect 1.0 0.0)
                               (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

; -- 

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

(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-bottom
            (transform-painter painter1
                               (make-vect 0.0 0.0)
                               (make-vect 1.0 0.0)
                               split-point))
          (paint-top
            (transform-painter painter2
                               split-point
                               (make-vect 1.0 0.5)
                               (make-vect 0.0 1.0))))
      (lambda (frame)
        (paint-bottom frame)
        (paint-top frame)))))

(define (below-2 painter1 painter2)
  (rot-180 (rot-270 
    (beside
      (flip-horiz (rot-270 painter1)) 
      (flip-horiz (rot-270 painter2))))))

; I won't do the wave painter, I have no data
(define frame (make-frame origin (scale-vect 2 xunit) yunit))
(print "-- X frame --")
(draw-X frame)
(print "-- flip-horz frame --")
((flip-horiz draw-X) frame)
(print "-- below X diamond --")
((below draw-X diamond) frame)
(print "-- below-2 X diamond --")
((below-2 draw-X diamond) frame)
