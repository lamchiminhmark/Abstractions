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