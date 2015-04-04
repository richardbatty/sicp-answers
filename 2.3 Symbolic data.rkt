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

;; Example: Symbolic Differentiation

;; Constructors
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        (else (list '** base exponent))))

;; Selectors
(define (addend sum)
  (cadr sum))

(define (augend sum)
  (caddr sum))

(define (multiplier product) (cadr product))

(define (multiplicand product) (caddr product))

(define (base exponential) (cadr exponential))

(define (exponent exponential) (caddr exponential))

;; Interegators
(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (sum? expression)
  (and (pair? expression) (eq? (car expression) '+)))

(define (product? expression)
  (and (pair? expression) (eq? (car expression) '*)))

(define (variable? expression) (symbol? expression))

(define (same-variable? var1 var2)
  (and (variable? var1)
       (variable? var2)
       (eq? var1 var2)))

(define (exponentiation? expression)
  (and (pair? expression) (eq? (car expression) '**)))

;; Differentiate
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product 
           (multiplier exp)
           (deriv (multiplicand exp) var))
          (make-product 
           (deriv (multiplier exp) var)
           (multiplicand exp))))
        ((exponentiation? exp)
         (make-product
          (exponent exp)
          (make-exponentiation (base exp)
                               (- (exponent exp) 1))))
        (else (error "unknown expression 
                      type: DERIV" exp))))

;; Exercise 2.56 - added handling exponentials in above code.

;; To evaluate
(display "*************")
(newline)
(deriv '(** (* 2 x) 3) 'x)
;; Started at 21:23. Finished 22:13