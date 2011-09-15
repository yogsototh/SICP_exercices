(load "displaylib.scm")
(title "Exercise 2.42")
(doc "The eight-queens puzzle asks how to place eight queenson a chessboard so that no queen is in check from any other(i.e., no two queens are in the same row, column, or diagonal). One possible solution is shown in figure 2.8. One way to solve the puzzle is to work across the board, placing a queen in each column. Once we have placed k - 1 queens, we must place the kth queen in a position where it does not check any of the queens already on the board. We can formulate this approach recursively: Assume that we have already generated the sequence of all possible ways to place k - 1 queens in the first k - 1 columns of the board. For each of these ways, generate an extended set of positions by placinga queen in each row of the kth column. Now filter these, keeping only the positions for which the queen in the kth column is safe with respect to the other queens. This producesthe sequence of all ways to place k queens in the first k columns. By continuing this process, we will produce not only one solution, but all solutions to the puzzle.\n\n  We implement this solution as a procedure queens, which returns a sequence of all solutions to the problem of placing n queens on an n× n chessboard. Queens has an internal procedurequeen-cols that returns the sequence of all ways to place queens in the first k columns of the board.\n\n  In this procedure rest-of-queens is a way to place k - 1 queens in thefirst k - 1 columns, and new-row is a proposed row in whichto place the queen for the kth column. Complete the programby implementing the representation for sets of board positions, including the procedure adjoin-position, which adjoinsa new row-column position to a set of positions, and empty-board, which represents an empty set of positions. You mustalso write the procedure safe?, which determines for a set of positions, whether the queen in the kth column is safe with respect to the others. (Note that we need only check whether the new queen is safe -- the other queens are already guaranteed safe with respect to each other.) ")

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
      (list (empty-board))
      (filter
        (lambda (positions) (safe? k positions))
        (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

; Data structure definition
; a partial board is a list of int
; (3 5) represent two columns of the board (in the direction you want)
; where a queen is on the row 3 and on the row 5.
; etc...
; a complete board is simple a partial board of good size.

(define (empty-board) nil)
(define (enumerate-interval start stop) (seq start stop))
(define (adjoin-position new-row k rest-of-queens)
  (cons new-row rest-of-queens))

(define (one-equals position positions)
  (accumulate 
    (lambda (b c) (or b c)) 
    #f
    (map (lambda (x) (= x position)) positions)))


(define (safe? k positions)
  (let ((current-index (car positions)))
    (not
    (or 
      ; no queen on same line
      (one-equals current-index (cdr positions)) 
      ; no queen on same positive diagonal
      (one-equals current-index (positive-diags (cdr positions) 1)) 
      ; no queen on same negative diagonal
      (one-equals current-index (negative-diags (cdr positions) 1)))))) 
; 
(define (positive-diags l n)
  (if (null? l)
    nil
    (cons (+ (car l) n) (positive-diags (cdr l) (+ n 1)))))

(define (negative-diags l n)
  (if (null? l)
    nil
    (cons (- (car l) n) (negative-diags (cdr l) (+ n 1)))))

(display "(safe? 0 (list 1 4 7))")(newline)
(display (safe? 0 (list 1 4 7)))(newline)
(display "(safe? 0 (list 1 4 1))")(newline)
(display (safe? 0 (list 1 4 1)))(newline)
(display "(queens 4)")(newline)
(display (queens 4))(newline)
(display "(queens 8)")(newline)
(display (queens 8))(newline)
