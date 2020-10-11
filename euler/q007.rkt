#lang racket
#|
By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.

What is the 10 001st prime number?
|#
(define (divable? a div)
  (= 0 (remainder a div)))

(define primes '(2))

(define (isPrime? x)
  (define (helper x ls)
    (cond
      [(empty? ls) (set! primes (cons x primes)) #t]
      [(divable? x (car ls)) #f]
      [else (helper x (cdr ls))]))
  (if (< x 2)
      #f
      (helper x primes)))

(define (euler-7-a n)
  (define (helper x k)
    (let* ( [wasPrime (isPrime? x)]
           [count (if wasPrime (add1 k) k)])
      (cond
        [(>= k n) (car primes)]
        [else (helper (+ x 2) count)])))
  (helper 3 1))

(euler-7-a 10001)