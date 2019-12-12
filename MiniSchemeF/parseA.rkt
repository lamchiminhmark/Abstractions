#lang racket

(provide parse lit-exp? lit-exp-num var-ref? var-ref-symbol)

(define parse (lambda (input)
                (cond
                  [(number? input) (new-lit-exp input)]
                  [(symbol? input) (new-var-ref input)]
                  [else (error 'parse "Invalid syntax ~s" input)])))

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