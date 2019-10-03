#lang racket
(require "TreeDatatype.rkt")

;(define index (lambda (a lat)
;                (foldr (lambda (i el) (if (eq? a el) i )) 0 lat)))

(define replace (lambda (a b lat)
                  (foldr (lambda (el accu) (if (eq? el a) (cons b accu) (cons el accu))) null lat)))

(define weigh (lambda (bags)
                (foldl (lambda (el accu) (+ accu (cadr el))) 0 bags)))

(define myBags '((duffle 8) (garment-bag 2) (briefcase 5) (valise 7) (steamer-trunk 65) ))

(define heaviest (lambda (bags)
                   (car (foldl (lambda (el accu) (if (> (cadr el) (cadr accu)) el accu)) '(null -inf.0) bags))))

(define allSum (lambda (tr)
                   (cond
                     [(empty-tree? tr) 0]
                     [else (apply + (cons (value tr) (map allSum (list-of-children tr))))])))

(define childSum (lambda (tr)
                   (cond
                     [(empty-tree? tr) 0]
                     [(leaf? tr) 0]
                     [else (apply + (map value (list-of-children tr)))])))

(define add1 (lambda (a) (+ 1 a)))

(define visitTree (lambda (f tr)
                    (cond
                      [(empty-tree? tr) (empty-tree)]
                      [else (non-empty-tree (f (value tr)) (map (lambda (ch) (visitTree f ch)) (list-of-children tr)))])))

(define sizeof (lambda (tr)
                   (cond
                     [(empty-tree? tr) 0]
                     [else (apply + (cons 1 (map sizeof (list-of-children tr))))])))

(define height (lambda (tr)
                   (cond
                     [(empty-tree? tr) -1]
                     [(leaf? tr) 0]
                     [else (+ 1 (apply max (map height (list-of-children tr))))])))

(define preorder (lambda (tr)
               (cond
                 [(empty-tree? tr) null]
                 [else (cons (value tr) (apply append (map preorder (list-of-children tr))))])))

(define postorder (lambda (tr)
               (cond
                 [(empty-tree? tr) null]
                 [(leaf? tr) (list (value tr))]
                 [else (apply append (cons (map postorder (list-of-children tr)) (list (list (value tr)))))])))
