#lang racket
(define atom? (lambda (exp)
                (not (list? exp))))

(define lat? (lambda (exp)
               (cond
                 [(atom? exp) #f]
                 [(null? exp) #t]
                 [(atom? (car exp)) (lat? (cdr exp))]
                 [else #f])))

(define not-lat? (lambda (exp)
                   (cond
                     [(atom? exp) #t]
                     [(null? exp) #f]
                     [(list? (car exp)) #t]
                     [else (not-lat? (cdr exp))])))

(define list-of-ints? (lambda (exp)
                        (cond
                          [(atom? exp) #f]
                          [(null? exp) #t]
                          [(integer? (car exp)) (list-of-ints? (cdr exp))]
                          [else #f])))

(define list-of-same? (lambda (pred exp)
                        (cond
                          [(atom? exp) #f]
                          [(null? exp) #t]
                          [(pred (car exp)) (list-of-same? pred (cdr exp))]
                          [else #f])))

(define list-of-same2 (lambda (pred)
                        (lambda (exp)
                          (cond
                          [(atom? exp) #f]
                          [(null? exp) #t]
                          [(pred (car exp)) (list-of-same? pred (cdr exp))]
                          [else #f]))))

(define contains? (lambda (thing lat)
                   (cond
                     [(null? lat) #f]
                     [(eq? thing (car lat)) #t]
                     [else (contains? thing (cdr lat))])))

(define allmembers (lambda (lat1 lat2)
                     (cond
                       [(null? lat1) #t]
                       [else (and (contains? (car lat1) lat2) (allmembers (cdr lat1) lat2))])))

(define rember2 (lambda (a lat)
                  (cond
                    [(null? lat) '()]
                    [(eq? a (car lat)) (cons a (remove a (cdr lat) eq?))]
                    [else (cons (car lat) (rember2 a (cdr lat)))])))

(define rember-pair (lambda (a lat)
                      (cond
                        [(null? lat) '()]
                        [(null? (cdr lat)) lat]
                        [(and (eq? a (car lat)) (eq? a (cadr lat))) (rember-pair a (cdr (cdr lat)))]
                        [else (cons (car lat) (rember-pair a (cdr lat)))])))

(define duplicate (lambda (n exp)
                    (cond
                      [(= n 0) '()]
                      [else (cons exp (duplicate (- n 1) exp))])))

(define largest (lambda (lat)
                  (cond
                    [(null? lat) -inf.0]
                    [(> (car lat) (largest (cdr lat))) (car lat)]
                    [else (largest (cdr lat))])))

(define index (lambda (a lat)
                (cond
                  [(null? lat) -1]
                  [(eq? a (car lat)) 0]
                  [else (+ 1 (if (= (index a (cdr lat)) -1) -2 (index a (cdr lat))))])))