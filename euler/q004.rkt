#lang racket

#|
A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.

Find the largest palindrome made from the product of two 3-digit numbers.
|#

(define (rev-num num)
  (define (helper n revnum)
    (let ([rem (remainder n 10)])
      (cond
        [(> n 0) (helper (/ (- n rem) 10) (+ (* 10 revnum) rem))]
        [else revnum])))
  (helper num 0))

(define (isPal? n)
  (= n (rev-num n)))

(define (euler-4-a high low)
  (define (helper a b cur)
    (let ([prod (* a b)])
    (cond
      [(< a low) cur]
      [(< b low) (helper (sub1 a) high cur)]
      [else (helper a (sub1 b) (if (and (isPal? prod) (> prod cur))
                                   prod
                                   cur))])))
  (helper high high 0))

(euler-4-a 999 100)