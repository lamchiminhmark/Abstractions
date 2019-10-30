#lang racket
(define rember-k (lambda (a lat k)
                   (cond
                     [(null? lat) (k null)]
                     [(eq? a (car lat)) (k (cdr lat))]
                     [else (rember-k a (cdr lat) (lambda (y) (k (cons (car lat) y))))])))

(define atom? (lambda (x) (not (list? x))))

(define index-k (lambda (a lat k)
                  (cond
                    [(null? lat) (k -1)]
                    [(eq? a (car lat)) (k 0)]
                    [else (index-k a (cdr lat) (lambda (x) (if (= x -1) x (k (+ 1 x)))))])))

; number not rounding to integer
(define max-k (lambda (L k)
                (cond
                  [(null? L) (k -inf.0)]
                  [(atom? (car L)) (max-k (cdr L) (lambda(y) (k (max y (car L)))))]
                  [else (max-k (car L) (lambda (y)
                                         (k (max-k (cdr L) (lambda (z) (k (max z y)))))))])))

(define replace-k (lambda (old new L k)
                    (cond
                      [(null? L) (k null)]
                      [(atom? (car L)) (let
                                           ([next (if (eq? (car L) old) new (car L))])
                                         (replace-k old new (cdr L) (lambda (y) (k (cons next y)))))]
                      [else (replace-k old new (car L) (lambda (y)
                                                     (k (replace-k old new (cdr L) (lambda (z) (cons y z))))))])))

;; backtracking
(define backtrack (lambda (ss goal nums sofar)
                    (cond
                      [(= ss goal) sofar]
                      [(> ss goal) null]
                      [(null? nums) null]
                      [else (let
                                ([res (backtrack (+ ss (car nums)) goal (cdr nums) (cons (car nums) sofar))])
                              (if (null? res) (backtrack ss goal (cdr nums) sofar) res))]
                      )))

(define subsetSum (lambda (goal nums)
                    (backtrack 0 goal nums null)))

(define prefix? (lambda (L1 L2)
                  (cond
                    [(null? L1) #t]
                    [(null? L2) #f]
                    [(eq? (car L1) (car L2)) (prefix? (cdr L1) (cdr L2))]
                    [else #f])))

(define ok? (lambda (X A)
              (cond
                [(> (car X) 3) #f]
                [(null? A) #t]
                [(prefix? X A) #f]
                [else (ok? (append X (list (car A))) (cdr A))])))

(define backtrack-noRepeat (lambda (current numsLeft sofar)
                             (cond
                               [(= 0 numsLeft) sofar]
                               [(< numsLeft 0) null]
                               [(ok? (list current) sofar)
                                (let
                                    ([res (backtrack-noRepeat 1 (- numsLeft 1) (cons current sofar))])
                                  (if (null? res) (backtrack-noRepeat (+ current 1) numsLeft sofar) res))]
                               [else (backtrack-noRepeat (+ current 1) numsLeft sofar)])))

(define noRepeat (lambda (n)
                   (backtrack-noRepeat 1 n null)))
