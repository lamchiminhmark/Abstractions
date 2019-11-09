#lang racket

(provide parse lit-exp? lit-exp-num var-ref? var-ref-symbol
         app-exp? app-exp-proc app-exp-args)

(define parse (lambda (input)
                (cond
                  [(number? input) (new-lit-exp input)]
                  [(symbol? input) (new-var-ref input)]
                  [(not (pair? input)) (error 'parse "Invalid syntax ~s" input)]
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

; app-exp cons
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