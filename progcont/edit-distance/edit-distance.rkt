#lang racket
(require readline)

(define call-count 0)
(define (cache-get ls key)
  (let helper ([ls ls] [key key])
    (cond [(empty? ls) empty]
          [else (let ([k (caar ls)]
                      [v (cadar ls)])
                  (if (equal? key k) v (helper (cdr ls) key)))])))

(define (edit-distance x y)
  (let ([ls '()])
    (let ed ([x (string->list x)] [y (string->list y)])
      (let ([res (cache-get ls (list x y))])
        (cond [(equal? empty res)
               (set! call-count (add1 call-count))
               (let ([val (cond [(empty? x) (length y)]
                                [(empty? y) (length x)]
                                [else (let ([x0 (car x)]
                                            [y0 (car y)]
                                            [xs (cdr x)]
                                            [ys (cdr y)])
                                        (if (equal? x0 y0)
                                            (ed xs ys)
                                            (min (add1 (ed x ys))
                                                 (add1 (ed xs y))
                                                 (add1 (ed xs ys)))))])])
                 (set! ls (cons (cons (list x y) (list val)) ls)) val)]
              [else res])))))
(set! call-count 0)
(edit-distance "Grave" "Groovy")
(printf "call-count ~a ~n" call-count)