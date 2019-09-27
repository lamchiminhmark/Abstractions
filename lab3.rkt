#lang racket

(define firsts (lambda (llyst)
                       (map car llyst)))

(define rests (lambda (llyst)
                      (map cdr llyst)))

(define pair-up (lambda (vec1 vec2)
                  (cond
                    [(null? vec1) null]
                    [else (cons (list (car vec1) (car vec2)) (pair-up (cdr vec1) (cdr vec2)))])))

(define addvec (lambda (vec1 vec2)
                 (let
                     ([pairedList (pair-up vec1 vec2)])
                   (map (lambda (vec) (apply + vec)) pairedList))))

(define dot-product (lambda (vec1 vec2)
                 (let
                     ([pairedList (pair-up vec1 vec2)])
                   (apply + (map (lambda (vec) (apply * vec)) pairedList)))))

(define dot-row (lambda (vec mat)
                  (map (lambda (row) (dot-product vec row)) mat)))

(define transpose (lambda (mat)
                    (cond
                      [(null? (car mat)) null]
                      [else (cons (firsts mat) (transpose (rests mat)))])))

(define matmult (lambda (mat1 mat2)
                  (letrec
                      ([trans-mat2 (transpose mat2)]
                       [matmult-helper (lambda (mat1 mat2)
                                         (map (lambda (vec) (dot-row vec mat2)) mat1))])
                    (matmult-helper mat1 trans-mat2))))

(define flatten (lambda (L)
                  (cond
                    [(not (list? L)) (list L)]
                    [(null? L) null]
                    [else (apply append (map flatten L))])))

(define sum (lambda (L)
              (cond
                [(null? L) 0]
                [(list? L) (apply + (map sum L))]
                [else L])))

(define add1 (lambda (x) (+ 1 x)))

(define map-to (lambda (f L)
                 (cond
                   [(null? L) null]
                   [(list? L) (map (lambda (smallL) (map-to f smallL)) L)]
                   [else (f L)])))

(define or-proc (lambda lat
                  (cond
                    [(null? lat) #f]
                    [(eq? #t (car lat)) #t]
                    [else (apply or-proc (cdr lat))])))

(define element-of? (lambda (a L)
                      (cond
                        [(null? L) #f]
                        [(list? L) (apply or-proc (map (lambda (smallL) (element-of? a smallL)) L))]
                        [(eq? a L) #t]
                        [else #f])))