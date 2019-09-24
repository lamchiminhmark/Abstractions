#lang racket
(define merge (lambda (lat1 lat2)
                (cond
                  [(null? lat1) (if (null? lat2) null lat2)]
                  [(null? lat2) lat1]
                  [(< (car lat1) (car lat2)) (cons (car lat1) (merge (cdr lat1) lat2))]
                  [else (cons (car lat2) (merge lat1 (cdr lat2)))])))

(define order-insert (lambda (a lat)
                 (cond
                   [(null? lat) (list a)]
                   [(< a (car lat)) (cons a lat)]
                   [else (cons (car lat) (order-insert a (cdr lat)))])))

(define sort (lambda (lat)
               (cond
                 [(null? lat) null]
                 [else (order-insert (car lat) (sort (cdr lat)))])))

(define starts-with? (lambda (small big)
                       (cond
                         [(null? small) #t]
                         [(null? big) #f]
                         [(eq? (car small) (car big)) (starts-with? (cdr small) (cdr big))]
                         [else #f])))

(define contains-sublist? (lambda (sublist biglist)
               (cond
                 [(null? biglist) #f]
                 [(starts-with? sublist biglist) #t]
                 [else (contains-sublist? sublist (cdr biglist))])))

(define rember-sublist (letrec (
                                  [remove (lambda (lat1 lat2)
                                            (cond
                                              [(null? lat1) lat2]
                                              [else (remove (cdr lat1) (cdr lat2))]))])
                          (lambda (sublist biglist)
                          (cond
                             [(null? biglist) null]
                             [(starts-with? sublist biglist) (remove sublist biglist)]
                             [else (cons (car biglist) (rember-sublist sublist (cdr biglist)))]))))

(define phone-number (lambda (person phone-book)
                       (let ([entry (if (null? phone-book) null (car phone-book))])
                       (cond
                         [(null? phone-book) 'disconnected]
                         [(eq? person (car entry)) (cadr entry)]
                         [else (phone-number person (cdr phone-book))]))))

(define person (lambda (phone-number phone-book)
                       (let ([entry (if (null? phone-book) null (car phone-book))])
                       (cond
                         [(null? phone-book) 'disconnected]
                         [(eq? phone-number (cadr entry)) (car entry)]
                         [else (person phone-number (cdr phone-book))]))))