#lang racket
(require "env.rkt")
(require "parseA.rkt")
(provide eval-exp)

(define eval-exp (lambda (tree env)
                   (cond
                     [(lit-exp? tree) (lit-exp-num tree)]
                     [(var-ref? tree) (lookup env (var-ref-symbol tree))]
                     [else (error 'eval-exp "Invalid tree: ~s" tree)])))