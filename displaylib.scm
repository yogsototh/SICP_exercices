; -- Defs --
(define nil ())

; -- accumulate (equivalent to foldr)
(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))

; -- define flatmap
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

; === Display nicely long strings ===

; search index to cut the line.
(define (rec-last-newline s n)
  (if (= n 0)
    0
    (let ((current-car (substring s n (+ n 1))))
      (if (string=? "\n" current-car)
        n
        (rec-last-newline s (- n 1))))))

(define (last-newline s n)
  (let ((index (rec-last-newline s n)))
    (if (= index 0)
      n
      index)))
  

(define (rec-last-special s n)
  (if (= n 0)
    0
    (let ((current-car (substring s n (+ n 1))))
      (if (not 
            (or 
              (and 
                (string>? current-car "A")
                (string<? current-car "Z"))
              (and
                (string>? current-car "a")
                (string<? current-car "z"))
              (and 
                (string>? current-car "0")
                (string<? current-car "9"))
              (string=? current-car "-")))
        n
        (rec-last-special s (- n 1))))))

(define (last-special s n)
  (let ((index (rec-last-special s n)))
    (if (= index 0)
      n
      index)))

(define (break-index s n)
  (let ((last-special-index (last-special s n))
        (last-newline-index (last-newline s n)))
        (min last-special-index last-newline-index)))

; wrap naturally a long string to 60 char wide
(define (wrap s)
  (if (< (string-length s) 60)
    s
    (let ((index (break-index s 60)))
      (string-append
        (substring s 0 index)
        (string "\n")
        (wrap (substring s (+ index 1) (string-length s))))
      )))

; print nicely something (wrap + newline)
(define (print s)
  (display (wrap s))(newline))

(define (title s)
  (newline)
  (display "             ")(print s)
  (newline))

(define (doc s)
  (display "------------------------------------------------------------")
  (newline)
  (print s)
  (display "------------------------------------------------------------")
  (newline))

; define seq (useful to count, should be called range)
(define (seq start stop)
  (if (> start stop)
    (list)
    (cons start (seq (+ start 1) stop))))

