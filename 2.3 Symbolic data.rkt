#lang racket

;;; *********** 2.3 Symbolic Data **********

;; Exercise 2.53
(list 'a 'b 'c)
'(a b c)

(list (list 'george))
'((george))

(cdr '((x1 x2) (y1 y2)))
'((y1 y2))

(cadr '((x1 x2) (y1 y2)))
'(y1 y2)

(pair? (car '(a short list)))
false

(memq 'red '((red shoes) (blue socks)))
false

(memq 'red '(red shoes blue socks))
'(red shoes blue socks)


;; Exercise 2.54

(define (equal? a b)
  (or (eq? a b)
      (and (eq? (car a)
                (car b))
           (equal? (cdr a)
                   (cdr b)))))

;; Mine was too simple. Bill the Lizard's:
(define (bill-the-lizard-equal? p1 p2)
  (cond ((and (null? p1) (null? p2)) #t)
        ((or (null? p1) (null? p2)) #f)
        ((and (pair? p1) (pair? p2))
         (and (equal? (car p1) (car p2))
              (equal? (cdr p1) (cdr p2))))
        ((or (pair? p1) (pair? p2)) #f)
        (else (eq? p1 p2))))

;; Exercise 2.55
(car ''abracadabra)
;; Same as
(car (quote (quote abracadabra)))
;; And because (quote abracadabra) is itself quoted, it is just a list of symbols, the two symbols it contains
;; are quote and abracadabra
;; That means the car of '(quote abracadabra) is quote.

;; To evaluate
(display "*************")

