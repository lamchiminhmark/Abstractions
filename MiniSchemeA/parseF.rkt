#lang racket

(provide parse lit-exp? lit-exp-num var-ref? var-ref-symbol
         app-exp? app-exp-proc app-exp-args
         if-exp? if-exp-cond if-exp-true if-exp-false
         let-exp? let-exp-symbols let-exp-vals let-exp-body
         lambda-exp? lambda-exp-params lambda-exp-body
         new-closure closure? closure-env)

(define parse (lambda (input)
                (cond
                  [(number? input) (new-lit-exp input)]
                  [(symbol? input) (new-var-ref input)]
                  [(not (pair? input)) (error 'parse "Invalid syntax ~s" input)]
                  [(eq? (car input) 'if)
                   (new-if-exp (parse (cadr input)) (parse (caddr input)) (parse (cadddr input)))]
                  [(eq? (car input) 'let)
                   (new-let-exp (cdr input))]
                  [(eq? (car input) 'lambda)
                   (new-lambda-exp (cdr input))]
                  [else (new-app-exp (parse (car input)) (map parse (cdr input)))])))

; lit-exp cons, recognizer and getter
(define new-lit-exp (lambda (input)
                      (list 'lit-exp input)))

(define lit-exp? (lambda (input)
                   (cond
                     [(not (list? input)) #f]
                     [else (eq? (car input) 'lit-exp)])))

(define lit-exp-num (lambda (input)
                      (cadr input)))

; var-ref cons, recognizer and getter
(define new-var-ref (lambda (input)
                      (list 'var-ref input)))

(define var-ref? (lambda (input)
                   (cond
                     [(not (list? input)) #f]
                     [else (eq? (car input) 'var-ref)])))

(define var-ref-symbol (lambda (input)
                      (cadr input)))

; app-exp cons, recognizer, and getter
(define new-app-exp (lambda (proc args)
                      (list 'app-exp proc args)))

(define app-exp? (lambda (input)
                   (cond
                     [(not (list? input)) #f]
                     [else (eq? (car input) 'app-exp)])))

(define app-exp-proc (lambda (app)
                       (cadr app)))

(define app-exp-args (lambda (app)
                       (caddr app)))

; if-exp cons
(define new-if-exp (lambda (cond if-true if-false)
                     (list 'if-exp cond if-true if-false)))

(define if-exp? (lambda (input)
                   (cond
                     [(not (list? input)) #f]
                     [else (eq? (car input) 'if-exp)])))

(define if-exp-cond (lambda (exp)
                       (cadr exp)))

(define if-exp-true (lambda (exp)
                       (caddr exp)))

(define if-exp-false (lambda (exp)
                       (cadddr exp)))

; let-exp
(define new-let-exp (lambda (input)
                      (list 'let-exp (map car (car input))
                            (map parse (map cadr (car input)))
                            (parse (cadr input)))
                      ))

(define let-exp? (lambda (input)
                   (cond
                     [(not (list? input)) #f]
                     [else (eq? (car input) 'let-exp)])))

(define let-exp-symbols (lambda (input)
                          (cadr input)))

(define let-exp-vals (lambda (input)
                          (caddr input)))

(define let-exp-body (lambda (input)
                          (cadddr input)))

; closure
(define new-closure (lambda (params body env)
                      (list 'closure params body env)))

(define closure? (lambda (input)
                   (cond
                     [(not (list? input)) #f]
                     [else (eq? (car input) 'closure)])))

(define closure-env (lambda (input)
                      (cadddr input)))

; lambda-exp
(define new-lambda-exp (lambda (input)
                         (list 'lambda-exp (car input) (parse (cadr input)))))

(define lambda-exp? (lambda (input)
                   (cond
                     [(not (list? input)) #f]
                     [else (eq? (car input) 'lambda-exp)])))

(define lambda-exp-params (lambda (input)
                          (cadr input)))

(define lambda-exp-body (lambda (input)
                          (caddr input)))