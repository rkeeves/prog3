#lang racket

#|
2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.

What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
|#
(define (gcd a b)
  (if (= 0 b)
      a
      (gcd b (remainder a b))))

(define (euler-5-a to)
  (define (helper ls accu)
    (if (empty? ls)
        accu
        (helper (cdr ls) (* accu (/ (car ls) (gcd accu (car ls)))))))
  (helper (build-list to add1) 1))

(euler-5-a 20)