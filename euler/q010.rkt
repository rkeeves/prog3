#lang racket
#|
The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

Find the sum of all the primes below two million.
|#

(require data/bit-vector)

(define MAX 2000000)



(define (make-bv bits-count)
  (cons bits-count (make-bit-vector bits-count #t)))

(define (sieve len-bv)
  (define bits-count (car len-bv))
  (define bits (cdr len-bv))
  (bit-vector-set! bits 0 #f)
  (bit-vector-set! bits 1 #f)
  
  (define (kill-products-of step n)
    (cond
      [(>= n bits-count) #f]
      [else (bit-vector-set! bits n #f) (kill-products-of step (+ n step))]))
  
  (define (test-prime n)
    (cond
      [(>= n bits-count) len-bv]
      [(bit-vector-ref bits n) (kill-products-of n (+ n n)) (test-prime (add1 n))]
      [else (test-prime (add1 n))]))
  
  (test-prime 0))

(define (sum len-bv)
  (define bits-count (car len-bv))
  (define bits (cdr len-bv))
  (define (helper n accu)
    (cond
      [(>= n bits-count) accu]
      [else (helper (add1 n) (if (bit-vector-ref bits n) (+ accu n) accu))]))
  (helper 0 0))

(sum (sieve (make-bv 10)))

(sum (sieve (make-bv MAX)))