#lang racket

#|
The sum of the squares of the first ten natural numbers is,
  1^2 + 2^2 + ... + 10^2 = 385
The square of the sum of the first ten natural numbers is,
  (1+2+...+10)^2=55^2=3025
Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is .
3025-385=2640

Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.
|#
(define (sq n) (* n n))

(define (euler-6-a to)
  (let* (
        [ls (build-list to add1)]
        [ls-sum-of-sqs (foldl (λ (n acc) (+ acc (sq n))) 0 ls)]
        [ls-sq-of-sum (sq (foldl + 0 ls))]
        )
    (cons (- ls-sq-of-sum ls-sum-of-sqs) ls-sq-of-sum)))

(euler-6-a 100)