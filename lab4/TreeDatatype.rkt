#lang racket
; definition of tree datatype
(provide tree? empty-tree? non-empty-tree? empty-tree non-empty-tree value list-of-children number-of-children leaf? makeTree)
(provide Empty T1 T2 T3 T4 T5 T6 T7 T8)


; recognizers
(define tree? (lambda (t) (or (empty-tree? t) (non-empty-tree? t))))

(define empty-tree? (lambda (x) (null? x)))                     

(define non-empty-tree? (lambda (x)
                          (cond
                            [(list? x) (eq? (car x) 'tree)]
                            [else #f])))



; constructors
(define empty-tree (lambda () null))

(define non-empty-tree (lambda (value list-of-children)
                                      (list 'tree value list-of-children)))

; Convenience constructor 
(define makeTree 
  (lambda l 
    (non-empty-tree (car l) (cdr l)))) 

; utilty functions
(define value (lambda (tree)
                (cond
                  [(empty-tree? tree) 'error]
                  [else (cadr tree)])))

(define list-of-children (lambda (tree)
                (cond
                  [(empty-tree? tree) 'error]
                  [else (caddr tree)])))

(define number-of-children 
  (lambda (tr)
                (cond
                  [(empty-tree? tr) 'error]
                  [else (length (list-of-children tr))]))) 

(define leaf? 
  (lambda (tr)
                (cond
                  [(empty-tree? tr) 'error]
                  [else (zero? (number-of-children tr))])))



; example trees 

(define Empty (empty-tree))
(define T1 (makeTree 50)) 
(define T2 (makeTree 22)) 
(define T3 (makeTree 10)) 
(define T4 (makeTree 5)) 
(define T5 (makeTree 17)) 
(define T6 (makeTree 73 T1 T2 T3)) 
(define T7 (makeTree 100 T4 T5)) 
(define T8 (makeTree 16 T6 T7)) 
