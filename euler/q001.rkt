#lang racket
#|
If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.

Find the sum of all the multiples of 3 or 5 below 1000.
|#
; [DATA]
(define N 1000)

; [UTILITY FUNCTIONS]
(define (div-by-3-or-5? v)
    (or (= 0 (remainder v 3))
        (= 0 (remainder v 5))))

; [SOLUTION A]
; Using lists
(define (euler-1-a n)
  (apply + (filter div-by-3-or-5? (build-list n values))))

(displayln "euler-1-a")
(euler-1-a N)

; [SOLUTION B]
; Not using lists
(define (euler-1-b max)
  (define (helper current accu)
    (cond
      [(< current 1) accu]
      [(div-by-3-or-5? current) (helper (- current 1) (+ accu current))]
      [else (helper (- current 1) accu)]))
  (helper max 0))

(displayln "euler-1-b")
(euler-1-b N)

; [SOLUTION B with syntactic sugar]
; Simply added a let binding and immediate eval
(define (euler-1-b-syntactic-sugar max)
  (let helper ([current max] [accu 0])
    (cond
      [(< current 1) accu]
      [(div-by-3-or-5? current) (helper (- current 1) (+ accu current))]
      [else (helper (- current 1) accu)])))

(displayln "euler-1-b-syntactic-sugar")
(euler-1-b-syntactic-sugar N)



