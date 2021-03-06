#lang racket
#|
Each new term in the Fibonacci sequence is generated by adding the previous two terms. By starting with 1 and 2, the first 10 terms will be:

1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...

By considering the terms in the Fibonacci sequence whose values do not exceed four million, find the sum of the even-valued terms.
|#
; [DATA]
(define N 4000000)

; [UTILITY FUNCTIONS]

; [SOLUTION A]
; Not using lists

(define (euler-2-a n)
  (define (helper a b accu)
    (cond
      [(< a n) (helper b (+ a b) (if (even? a) (+ accu a) accu))]
      [else accu]))
  (helper 0 1 0))

(euler-2-a N)