#lang racket

(require "utils.scm")

;;; Exercise 1.A

(((thrice thrice) 1+) 6)
; 33
  
(((thrice thrice) identity) compose)

; procedure compose

(((thrice thrice) square) 1)
; 1

;;(((thrice thrice) square) 2)
; 2^2^2^2^2^2^2^2^2^2^2^2^2^2^2^2^2^2^2^2^2^2^2^2^2^2^2^2
; Doesn't stop
