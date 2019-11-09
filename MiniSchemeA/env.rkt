
#lang racket
(provide environment? empty-env? extended-env? empty-env extended-env
         syms vals old-env lookup init-env new-prim-proc prim-proc? prim-proc-symbol)


; datatype definition

(define environment? (lambda (e) (or (empty-env? e) (extended-env? e))))

(define empty-env? (lambda (e)
                     (cond
                       [(list? e) (eq? (car e) 'empty-env)]
                       [else #f])))

(define extended-env? (lambda (e)
                        (cond
                          [(list? e) (eq? (car e) 'extended-env)]
                          [else #f])))

(define empty-env (lambda ()
                    (list 'empty-env)))

(define extended-env (lambda (syms vals old-env)
                       (list 'extended-env syms vals old-env)))

(define syms (lambda (env)
               (cond
                 [(extended-env? env) (cadr env)]
                 [else (error 'syms "bad environment")])))

(define vals (lambda (env)
               (cond
                 [(extended-env? env) (caddr env)]
                 [else (error 'vals "bad environment")])))

(define old-env (lambda (env)
                  (cond
                    [(extended-env? env) (cadddr env)]
                    [else (error 'old-env "bad environment")])))

(define the-empty-env (empty-env))

(define lookup (lambda (env sym)
                 (cond
                   ; Is using 'lookup here correct?
                   [(empty-env? env) (error 'lookup "No binding for ~s" sym)]
                   [(null? (syms env)) (lookup (old-env env) sym)]
                   [(eq? sym (car (syms env))) (car (vals env))]
                   [else (lookup (extended-env (cdr (syms env)) (cdr (vals env)) (old-env env)) sym)]
                   )))

; prim-proc cons and getter
(define new-prim-proc (lambda (p)
                        (list 'prim-proc p)))

(define prim-proc? (lambda (p)
                     (cond
                       [(not (list? p)) #f]
                       [else (eq? (car p) 'prim-proc)])))

(define prim-proc-symbol (lambda (p)
                           (cadr p)))

(define primitive-operators '(+ - * / add1 sub1 minus list build first rest empty?))

(define init-env (extended-env primitive-operators (map new-prim-proc primitive-operators)
                               (extended-env '(x y nil) '(10 23 ()) (empty-env))))



