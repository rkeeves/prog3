#lang racket
#|
The prime factors of 13195 are 5, 7, 13 and 29.

What is the largest prime factor of the number 600851475143 ?
|#
; [DATA]
(define N 600851475143)

; [UTILITY FUNCTIONS]
(define (smallest-divisor n)
  (define (helper divi)
    (cond
      [(> (* divi divi) n) n]
      [(= 0 (remainder n divi)) divi ]
      [else (helper (+ divi 1))]))
  (helper 2))

; [SOLUTION A]
; Not using lists

(define (euler-3-a n)
  (let ( [d (smallest-divisor n)] )
    (cond
      [(= d n) n]
      [else (euler-3-a (/ n d))])))

(euler-3-a N)