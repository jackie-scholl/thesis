#lang racket

(require racket/list)
(require racket/match)
(require rebellion/base/option)
(require rebellion/streaming/reducer)

; 'X and 'O


(define empty-board (make-list 9 absent))


(define (get-empties board)
	(define (isSpaceEmpty x) (absent? (list-ref board x)))
	(filter isSpaceEmpty (range 9))
)

(define (count-empties board) (length (get-empties board)))


(define (get-current-turn board)
	(if (= (modulo (count-empties board) 2) 0) 'O 'X)
)



(define (print-board board)


	(define (chunks-of n list) (sequence->list (in-slice n list)))
	(define (rows board) (chunks-of 3 board))

	(define (print-space p) 
		(match p [(present 'X) "X"] [(present 'O) "O"] [absent "_"])
	)

	(define (print-row row) (string-join (map print-space row)))

	(define print-board2 (string-join (map print-row (rows board)) "\n" #:after-last "\n"))

	(define game-state
		(match (get-end-state board)
			['NotOver (string-append "Game is not yet over; current turn " 
							(symbol->string (get-current-turn board)) "\n")]
			['Draw "Game ended in a draw\n"]
			[winner (string-append "some player won: " (symbol->string winner) " wee!!\n")]
		)
	)

	(string-append game-state print-board2 "\n")
)

(define (get-end-state board)
	(define all-lines
		(list
			(list 0 1 2) (list 3 4 5) (list 6 7 8) ; horizontal lines
			(list 0 3 6) (list 1 4 7) (list 2 5 8) ; vertical lines
			(list 0 4 8) (list 2 4 6) ; diagonal lines
		)
	)

	(define (test-line to-test)
		(match to-test
			[(list 'X 'X 'X) 'X]
			[(list 'O 'O 'O) 'O]
			;[(list 'O 'O 'O) 'O]
			[(list a b c) absent]
		)
	)

	(define (extracted-lines)
		(define (thing1 x) (list-ref board x))
		(define (thing2 temp-line) (map thing1 temp-line))
		(map thing2 all-lines)
	)

	(define (first-present my-line)
		(define (thing3 x) (if (null? x) absent (car x)))
		(thing3 (filter-not absent? my-line))
	)
	
	(define possible-winner (first-present (map test-line (extracted-lines))))

	(if
		(not (absent? possible-winner))
		possible-winner
		(if (eq? 0 (count-empties board)) 'Draw 'NotOver)
	)
)

(define (move board index)
	(if (present? (list-ref board index))
		(display "err")
		(list-set board index (present (get-current-turn board)))
	)
)

(define (move-best board) (move board (choose-move board)))


(define (score board player)

	(define end-state (get-end-state board))
	(define (recursive-score) (* 0.9 (score (move-best board) player)))
	(match end-state
		['Draw 0]
		['NotOver (recursive-score)]
		[winner (if (eq? winner player) 1 -1)]
	)
)

(define (choose-move board)
	(argmax
		(λ (c) (score (move board c) (get-current-turn board)))
		(get-empties board)
	)
)


;; MAIN:
(define sample-inputs (range 9))
(define sample-board (reduce-all (make-fold-reducer (λ (cur-board item) (move cur-board item)) empty-board) sample-inputs))

(define sample-board2
	(reduce-all
	(make-fold-reducer (λ (cur-board item) (move cur-board item))
					  empty-board)
	(range 3))
)

(define (iterate f x) (stream-cons x (iterate f (f x))))


(display "\n")
(display (print-board sample-board))
;(extract-lines sample-board)

;(check-winner sample-board)

(score sample-board2 'X)
(score sample-board2 'O)

(define sample3 (move (move empty-board 0) 4))

(define (helper board) (move board (choose-move board)))

(define mystream (stream-take (iterate helper sample3) 8))
(for ([board (in-stream mystream)]) (display (print-board board)))























