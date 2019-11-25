#lang racket
(require "env.rkt")
(require "parseF.rkt")
(provide eval-exp)

(define eval-exp (lambda (tree env)
                   (cond
                     [(lit-exp? tree) (lit-exp-num tree)]
                     [(var-ref? tree) (lookup env (var-ref-symbol tree))]
                     [(app-exp? tree) (apply-proc (let ([proc (app-exp-proc tree)])
                                                    (cond
                                                      [(primitive-operator? proc) (new-prim-proc (var-ref-symbol proc))]
                                                      [else (eval-exp proc env)]))
                                                  (map (lambda (t) (eval-exp t env)) (app-exp-args tree)))]
                     [(if-exp? tree) (apply-if tree env)]
                     [(let-exp? tree) (apply-let tree env)]
                     [(lambda-exp? tree) (new-closure (lambda-exp-params tree) (lambda-exp-body tree) env)]
                     [else (error 'eval-exp "Invalid tree: ~s" tree)])))

(define apply-proc (lambda (p arg-values)
                     (cond
                       [(prim-proc? p) (apply-primitive-op (prim-proc-symbol p) arg-values)]
                       [(closure? p) (let
                                         ([new-env (extended-env (lambda-exp-params p) arg-values (closure-env p))])
                                       (eval-exp (lambda-exp-body p) new-env))]
                       [else (error 'apply-proc "Bad procedure: ~s" p)])))

(define apply-if (lambda (if-exp env)
                   (cond
                     [(or (eq? (eval-exp (if-exp-cond if-exp) env) 0) (eq? (eval-exp (if-exp-cond if-exp) env) 'False))
                      (eval-exp (if-exp-false if-exp) env)]
                     [else (eval-exp (if-exp-true if-exp) env)])))

(define apply-let (lambda (let-exp env)
                    (let
                        ([new-env
                          (extended-env
                           (let-exp-symbols let-exp)
                           (map (lambda (tree) (eval-exp tree env)) (let-exp-vals let-exp))
                           env)])
                      (eval-exp (let-exp-body let-exp) new-env))))

(define _convert-bool (lambda (bool)
                        (if bool 'True 'False)))

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
                               [(eq? op 'first) (car (car arg-values))]                            
                               [(eq? op 'rest) (cdr (car arg-values))]                            
                               [(eq? op 'empty?) (eq? (car arg-values) (lookup init-env 'nil))]
                               [(eq? op 'equals?) (_convert-bool (eqv? (car arg-values) (cadr arg-values)))]
                               [(eq? op 'lt?) (_convert-bool (< (car arg-values) (cadr arg-values)))]
                               [(eq? op 'gt?) (_convert-bool (> (car arg-values) (cadr arg-values)))]
                               [(eq? op 'leq?) (_convert-bool (<= (car arg-values) (cadr arg-values)))]
                               [(eq? op 'geq?) (_convert-bool (>= (car arg-values) (cadr arg-values)))]
                               )))