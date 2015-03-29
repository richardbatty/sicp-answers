;;; Scheme code for Twenty-One Simulator [PS2 Fall '90]

(define (twenty-one player-strategy house-strategy)
  (let ((house-initial-hand (make-new-hand (deal))))
    (let ((player-hand
           (play-hand player-strategy
                      (make-new-hand (deal))
                      (hand-up-card house-initial-hand))))
      (if (> (hand-total player-hand) 21)
          0                                ; ``bust'': player loses
          (let ((house-hand 
                 (play-hand house-strategy
                            house-initial-hand
                            (hand-up-card player-hand))))
            (cond ((> (hand-total house-hand) 21)
                   1)                      ; ``bust'': house loses
                  ((> (hand-total player-hand)
                      (hand-total house-hand))
                   1)                      ; house loses
                  (else 0)))))))           ; player loses

(define (play-hand strategy my-hand opponent-up-card)
  (display "playing hand")
  (cond ((> (hand-total my-hand) 21) my-hand) ; I lose... give up
        ((strategy my-hand opponent-up-card) ; hit?
         (play-hand strategy
                    (hand-add-card my-hand (deal))
                    opponent-up-card))
        (else my-hand)))                ; stay


(define (deal) (make-card (+ 1 (random 10)) "hearts"))

(deal)

(define (make-new-hand first-card)
  (make-hand first-card first-card))

(define (make-hand up-card total)
  (cons up-card total))

(define (hand-up-card hand)
  (car hand))

(define (hand-total hand)
  (cdr hand))

(define (hand-add-card hand new-card)
  (make-hand (hand-up-card hand)
             (+ new-card (hand-total hand))))

(define (hit? your-hand opponent-up-card)
  (newline)
  (display "Opponent up card ")
  (display opponent-up-card)
  (newline)
  (display "Your Total: ")
  (display (hand-total your-hand))
  (newline)
  (display "Hit? ")
  (newline)
  (user-says-y?))


(define (user-says-y?) (eq? (read-char) #\y))

(twenty-one hit? hit?)

;;;;;;;;;;;;;; My work ;;;;;;;;;;;;;;

;; Problem 2

(define (stop-at num)
  (lambda (your-hand opponent-up-card)
    (< (hand-total your-hand) num)))

(twenty-one (stop-at 15) (stop-at 16))


;;; Problem 3

; Utils
(define (accumulate op initial seq)
  (if (null? seq)
      initial
      (op (car seq) (accumulate op initial (cdr seq)))))

(define (build-list start end proc)
  (if (> start end)
      '()
      (cons (proc start) (build-list (+ start 1) end proc))))


(define (count test seq)
  (accumulate (lambda (current next)
                (if (test current)
                    (+ 1 next)
                    next))
              0
              seq))

(define (display-and-return value)
  (display value)
  value)

(display-and-return 3)

; Playing the game
(define (play-n-games player-strategy house-strategy no-of-games)
  (build-list 1 no-of-games (lambda (x) (twenty-one player-strategy house-strategy))))

(define (test-strategy player-strategy house-strategy no-of-games)
  (count (lambda (n)
           (if (= 1 n)
               true
               false))
         (display-and-return (play-n-games player-strategy house-strategy no-of-games))))

(test-strategy (stop-at 16) (stop-at 15) 10)

;;; Problem 4

; Utils

(define (flip n)
  (if (= n 0)
      1
      0))

(define (alternator start-state)
  (let ((current-state start-state))
    (lambda ()
      (set! current-state (flip current-state))
      current-state)))

(define player (alternator 0))
(player)

(define (display-situation your-hand opponent-up-card)
  (newline)
  (display "Player: ")
  (display (player))
  (newline)
  (display "Opponent up card: ")
  (display opponent-up-card)
  (newline)
  (display "Your Total: ")
  (display (hand-total your-hand))
  (newline))

(define (watch-player strategy)
  (lambda (your-hand opponent-up-card)
    (display-situation your-hand opponent-up-card)
    (strategy your-hand opponent-up-card)))

(test-strategy (watch-player (stop-at 16))
               (watch-player (stop-at 15))
               4)


;;; Problem 5
(define (louis your-hand opponent-up-card)
  (let ((total (hand-total your-hand)))
    (cond ((< total 12) #t)
          ((> total 16) #f)
          ((= total 12) (< opponent-up-card 4))
          ((= total 16) (not (= opponent-up-card 10)))
          (else (> opponent-up-card 6)))))

(test-strategy louis (stop-at 15) 10)
(test-strategy louis (stop-at 16) 10)
(test-strategy louis (stop-at 17) 10)

;;; Problem 6
(define (both strategy-1 strategy-2)
  (lambda (your-hand opponent-up-card)
    (and (strategy-1 your-hand opponent-up-card)
         (strategy-2 your-hand opponent-up-card))))

(test-strategy louis (both (stop-at 19) louis) 10)


;;; Tutorial exercise 1

; *** Cards ***
(define (make-card value suit)
  (list value suit))

(define (card-value card)
  (first card))

(define (card-suit card)
  (last card))

; *** Card set ***
(define (card-set . cards)
  cards)

(define (first-card set)
  (first set))

(define (add-card new-card set)
  (append (list new-card) set))

; *** Hand ***

(define (make-new-hand card)
  (make-hand card (card-set card)))

(define (make-hand up-card set)
  (list up-card set))

(define (hand-add-card hand new-card)
  (make-hand (hand-up-card hand)
             (add-card new-card (hand-set hand))))

(define (hand-up-card hand)
  (first hand))

(define (hand-set hand)
  (last hand))

(define (hand-total hand)
  (accumulate + 0 (map (lambda (card)
                          (card-value card))
                        (hand-set hand))))

;; Other parts of the simulation that needed changing: the deal function.
;; Nothing else because of a good use of abstraction barriers.



;;; Tutorial exercise 2
; a

(define cards-in-a-suit
  (list 0 1 2 3 4 5 6 7 8 9 10 10 10))

(define (create-deck)
  (append-map (lambda (value)
             (list value value value value)) cards-in-a-suit))

;b 

(define (dec n)
  (- n 1))

(define (slice start end seq)
  (cond ((> start 0)
         (slice (dec start) (dec end) (cdr seq)))
        ((= end 0) '())
        (else (cons (car seq) (slice 0 (dec end) (cdr seq))))))

(define (first-half-deck deck)
  (slice 0 (quotient (length deck) 2) deck))

(define (second-half-deck deck)
  (slice (quotient (length deck) 2) (length deck) deck))

(define (shuffle deck)
  (let ((first-half (first-half-deck deck))
        (second-half (second-half-deck deck)))
    (append-map (lambda (first-half-card second-half-card)
                  (list first-half-card second-half-card))
                first-half second-half)))

(shuffle (create-deck))

; c
(define (split index seq)
  (if (> index (length seq))
      seq
      (list (slice 0 index seq)
            (slice index (length seq) seq))))

(split 5 (create-deck))

(define (random-recombine seq-1 seq-2)
  (let ((split-seq-1 (split 2 seq-1))
        (split-seq-2 (split 2 seq-2)))
      (append (first split-seq-1)
              (first split-seq-2)
              (random-recombine (last split-seq-1)
                                (last split-seq-2))
  )

(define (random-shuffle deck)
  (let ((first-half (first-half-deck deck))
        (second-half (second-half-deck deck)))
    ))))

(append (list 1 2) (list 3 4) (list 5 6))
    
