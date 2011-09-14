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
; --- frame segment ---
(define (make-segment start end) (list start end))
(define (start-segment s) (car s))
(define (end-segment s) (cadr s))

; -- defined function from SICP text
(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
      (origin-frame frame)
      (add-vect (scale-vect (xcor-vect v)
                            (edge1-frame frame))
                (scale-vect (ycor-vect v)
                            (edge2-frame frame))))))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
      (lambda (segment)
        (draw-line
          ((frame-coord-map frame) (start-segment segment))
          ((frame-coord-map frame) (end-segment segment))))
      segment-list)))
