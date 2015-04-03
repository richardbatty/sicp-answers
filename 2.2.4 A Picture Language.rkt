#lang racket
(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

(define (right-split painter n)
  (if (<= n 0)
      painter
      (beside painter (below (right-split painter (- n 1))
                             (right-split painter (- n 1))))))
;; Exercise 2.44
(define (up-split painter n)
  (if (<= n 0)
      painter
      (below painter (beside (up-split painter (- n 1))
                             (up-split painter (- n 1))))))

;; /exercise

(define (corner-split painter n)
  (if (<= n 0)
      painter
      (beside (below painter 
                     (up-split painter (- n 1)))
              (below (right-split painter (- n 1))
                     (corner-split painter (- n 1))))))
  
(define (square-of-four tl tr bl br)
  (lambda (painter)
    (below (beside (bl painter) (br painter))
           (beside (tl painter) (tr painter)))))

(define (flipped-pairs painter)
  ((square-of-four identity flip-vert identity flip-vert) painter))

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity rotate180 flip-vert)))
    (combine4 (corner-split painter n))))

;; Exercise 2.45
(define (split first-arrangement second-arrangement)
  (define (splitter painter n)
    (if (<= n 0)
      painter
      (first-arrangement painter
                         (second-arrangement (splitter painter (- n 1))
                                                     (splitter painter (- n 1))))))
  splitter)

(define my-right-split (split beside below))
(define my-up-split (split below beside))
;; /exercise



;; Exercise 2.46

;; Constructors and selectors
(define (my-make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

;; Vector arithmetic
(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1)
                (xcor-vect v2))
             (+ (ycor-vect v1)
                (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1)
                (xcor-vect v2))
             (- (ycor-vect v1)
                (ycor-vect v2))))

(define (scale-vect v scale)
  (make-vect (* scale (xcor-vect v))
             (* scale (ycor-vect v))))

;; Exercise 2.47

;; Frame constructor using lists
(define (make-frame-list origin edge1 edge2)
  (list origin edge1 edge2))

;; Frame selectors using list
(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (car (cdr frame)))

(define (edge2-frame frame)
  (car (cdr (cdr frame))))

;; Frame constructor using cons

(define (make-frame-cons origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (my-origin-frame frame)
  (car frame))

(define (my-edge1-frame frame)
  (car (cdr frame)))

(define (my-edge2-frame frame)
  (cdr (cdr frame)))

;; Exercise 2.48
(define (my-make-segment start-vector end-vector)
  (cons start-vector end-vector))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

;; Exercise 2.49
;; Useful points
(define bottom-left (make-vect 0 0))
(define top-left (make-vect 0 0.99))
(define top-right (make-vect 0.99 0.99))
(define bottom-right (make-vect 0.99 0))
(define mid-left (make-vect 0 0.5))
(define mid-top (make-vect 0.5 1))
(define mid-right (make-vect 1 0.5))
(define mid-bottom (make-vect 0.5 0))

;; Painters
(define outline-segments
    (list (make-segment bottom-left top-left)
          (make-segment top-left top-right)
          (make-segment top-right bottom-right)
          (make-segment bottom-right bottom-left)))

(define outline (segments->painter outline-segments))

(define x-segments
    (list (make-segment bottom-left top-right)
          (make-segment top-left bottom-right)))

(define x (segments->painter x-segments))

(define diamond-segments
    (list (make-segment mid-left mid-top)
          (make-segment mid-top mid-right)
          (make-segment mid-right mid-bottom)
          (make-segment mid-bottom mid-left)))

(define diamond (segments->painter diamond-segments))

;; Exercise 2.50
(define (my-flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

;; Skipped rotate180 and rotate270 as they will be similar

;; Exercise 2.51
(define (my-transform-painter 
         painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter (make-frame new-origin
                  (sub-vect (m corner1) 
                            new-origin)
                  (sub-vect (m corner2)
                            new-origin)))))))

(define (my-below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-bottom (my-transform-painter painter1
                                           (make-vect 0.0 0.0)
                                           (make-vect 1.0 0.0)
                                           split-point))
          (paint-top (my-transform-painter painter2
                                        split-point   
                                        (make-vect 1.0 0.5)
                                        (make-vect 0 1.0))))
      (lambda (frame)
        (paint-bottom frame)
        (paint-top frame)))))
                                         
(define (my-other-below painter1 painter2)
  (rotate90 (beside (rotate270 painter1) (rotate270 painter2))))

;; Exercise 2.52
; 1
(define wave-segments
 (list
  (make-segment
   (make-vect 0.006 0.840)
   (make-vect 0.155 0.591))
  (make-segment
   (make-vect 0.006 0.635)
   (make-vect 0.155 0.392))
  (make-segment
   (make-vect 0.304 0.646)
   (make-vect 0.155 0.591))
  (make-segment
   (make-vect 0.298 0.591)
   (make-vect 0.155 0.392))
  (make-segment
   (make-vect 0.304 0.646)
   (make-vect 0.403 0.646))
  (make-segment
   (make-vect 0.298 0.591)
   (make-vect 0.354 0.492))
  (make-segment
   (make-vect 0.403 0.646)
   (make-vect 0.348 0.845))
  (make-segment
   (make-vect 0.354 0.492)
   (make-vect 0.249 0.000))
  (make-segment
   (make-vect 0.403 0.000)
   (make-vect 0.502 0.293))
  (make-segment
   (make-vect 0.502 0.293)
   (make-vect 0.602 0.000))
  (make-segment
   (make-vect 0.348 0.845)
   (make-vect 0.403 0.999))
  (make-segment
   (make-vect 0.602 0.999)
   (make-vect 0.652 0.845))
  (make-segment
   (make-vect 0.652 0.845)
   (make-vect 0.602 0.646))
  (make-segment
   (make-vect 0.602 0.646)
   (make-vect 0.751 0.646))
  (make-segment
   (make-vect 0.751 0.646)
   (make-vect 0.999 0.343))
  (make-segment
   (make-vect 0.751 0.000)
   (make-vect 0.597 0.442))
  (make-segment
   (make-vect 0.597 0.442)
   (make-vect 0.999 0.144))
  ;; Smile
  (make-segment
   (make-vect 0.47 0.75)
   (make-vect 0.53 0.75))))

(define wave (segments->painter wave-segments))

; 2
; Already done earlier

; 3
(define (alternative-square-limit painter n)
  (let ((combine4 (square-of-four rotate270 rotate180 identity rotate90)))
    (combine4 (corner-split painter n))))

;; Things to evaluate
(paint (alternative-square-limit diagonal-shading 1))

