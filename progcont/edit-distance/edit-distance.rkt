#lang racket
(require readline)
#|
Before we get into the actual solution,
here's an example of memoization.

Without memoization, each time we call add-two-numbers,
it adds them together, but laso prints out a message.
|#
(define (add-two-numbers a b)
  (displayln "compute") (+ a b))
(add-two-numbers 1 1)
(add-two-numbers 1 1)
#|
With memoization, first we use a 'cache' hashtable.
Before we evaluate the actual add-two-numbers procedure,
we first lookup whether we already know the solution.
For this 'memory' we use the aforementioned hashtable.
hash-ref!'s last arg is a lambda expression, which gets evaluated
when the hashtable doesnt include the given key,
then before returning the value we store it back into the table.
|#
(define (memo f)
  (let ([ht (make-hash)])
    (λ (x y) (hash-ref! ht (cons x y) (λ () (f x y))))))

(define memoized-add-two-numbers (memo add-two-numbers))
(memoized-add-two-numbers 1 1)
(memoized-add-two-numbers 1 1)
#|
So we cause only one displayln,
because next time we don't have to actually evaluate add-two-numbers,
since its result is already cached.
|#
(define (edit-distance x y)
  (define (ed x y)
    (cond [(empty? x) (length y)]
          [(empty? y) (length x)]
          [else (let ([x0 (car x)]
                      [y0 (car y)]
                      [xs (cdr x)]
                      [ys (cdr y)])
                  (if (equal? x0 y0)
                      (ed xs ys)
                      (min (add1 (ed x ys))
                           (add1 (ed xs y))
                           (add1 (ed xs ys)))))]))
  ((memo ed) (string->list x) (string->list y)))

(define (get-num-from-user)
  (printf "Enter # of string pairs to analyze:")
  (string->number (read-line)))

(define (get-strings-from-user i res)
  (cond [(> i 0) (get-strings-from-user (sub1 i) (cons (cons (read-line) (read-line)) res))]
        [else res]))

(define (pair-to-distance pair)
  (let([a (car pair)]
       [b (cdr pair)])
    (edit-distance a b)))

(define (for-each-idx proc ls start-idx)
  (let helper([i start-idx] [ls ls])
    (unless (empty? ls) (proc i (car ls)) (helper (add1 i) (cdr ls)))))

(define (print-result i edit-res)
  (printf "~a. Edit Distance ~a" i edit-res))

(define (edit-distance-prog)
  (define (printer i ls)
    (unless (empty? ls) (printf "~a. Edit Distance ~a" i (car ls)) (printer (add1 i) (cdr ls))))
  (let* ([num (get-num-from-user)]
         [triplets  (map pair-to-distance (get-strings-from-user num '()))])
     (for-each-idx (λ (i edit-res) (print-result i edit-res)) triplets 1)))

(edit-distance-prog)