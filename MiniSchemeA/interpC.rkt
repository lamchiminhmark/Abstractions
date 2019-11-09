#lang racket
(require "env.rkt")
(require "parseC.rkt")
(provide eval-exp)

(define eval-exp (lambda (tree env)
                   (cond
                     [(lit-exp? tree) (lit-exp-num tree)]
                     [(var-ref? tree) (lookup env (var-ref-symbol tree))]
                     [(app-exp? tree) (apply-proc (new-prim-proc (var-ref-symbol (app-exp-proc tree)))
                                                  (map (lambda (t) (eval-exp t env)) (app-exp-args tree)))]
                     [else (error 'eval-exp "Invalid tree: ~s" tree)])))

(define apply-proc (lambda (p arg-values)
                     (cond
                       [(prim-proc? p) (apply-primitive-op (prim-proc-symbol p) arg-values)]
                       [else (error 'apply-proc "Bad procedure: ~s" p)])))

(define apply-primitive-op (lambda (op arg-values)
                             (cond
                               [(eq? op '+) (+ (car arg-values) (cadr arg-values))]
                               [(eq? op '-) (- (car arg-values) (cadr arg-values))]
                               [(eq? op '*) (* (car arg-values) (cadr arg-values))]
                               [(eq? op '/) (/ (car arg-values) (cadr arg-values))]
                               [(eq? op 'add1) (+ (car arg-values) 1)]
                               [(eq? op 'sub1) (- (car arg-values) 1)]                               
                               [(eq? op 'minus) (- 0 (car arg-values))]                            
                               [(eq? op 'list) (apply list arg-values)]                            
                               [(eq? op 'build) (cons (car arg-values) (cadr arg-values))]                            
                               [(eq? op 'first) (car arg-values)]
                               )))