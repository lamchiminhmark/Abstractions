#lang racket

(provide car$ cdr$ cons$)
(provide nth$ show$ +$ map$ append$)
(provide filter$ fold$ printn$ print$)

(define car$
  (lambda (s)
    (if (promise? s) (car$ (force s)) (force (car s)))))

(define cdr$
  (lambda (s)
    (if (promise? s) (cdr$ (force s)) (force (cdr s)))))

(define-syntax cons$
  (syntax-rules ()
    [(_ x y) (cons x (delay y))]))

(define nth$
  (lambda (n s)
    (if (zero? n) s
        (nth$ (sub1 n) (cdr$ s)))))

(define show$
  (lambda (n s)
    (if (zero? n) null
        (cons (car$ s) (show$ (sub1 n) (cdr$ s))))))

(define +$
  (lambda (s1 s2)
    (cons$ (+ (car$ s1) (car$ s2)) (+$ (cdr$ s1) (cdr$ s2)))))

(define map$
  (lambda (f s)
    (cons$ (f (car$ s)) (map$ f (cdr$ s)))))

(define append$
  (lambda (x y)
    (if (null? x) y
        (cons$ (car$ x) (append$ (cdr$ x) y)))))

(define filter$
  (lambda (f l)
    (let filter1$ ([l l])
      (cond [(null? l)null]
            [(f (car$ l)) (cons$ (car$ l) (filter1$ (cdr$ l)))]
            [else (filter1$ (cdr$ l))]))))

(define fold$
  (lambda (recur-case base-case base-test lyst)
    (let help-fold ([l lyst])
      (if (base-test l)
          base-case
          (recur-case (car$ l) (help-fold (cdr$ l)))))))

; print the first n members of stream s:
(define printn$
  (lambda (s n)
    (if (= 0 n)
        'done
        (begin
          (printf "~s~%" (car$ s))
          (printn$  (cdr$ s) (- n 1))))))

; print the stream s, pausing after each 10 to ask the user if the 
; printing should continue:
(define print$
  (lambda (s)
    (letrec ([printn
              (lambda (s n)
                (if (= 0 n)
                    s
                    (begin
                      (printf "~s " (car$ s))
                      (printn (cdr$ s) (- n 1)))))]
             [printrow
              (lambda (s)
                (printf "~%Want more? ")
                (if (eq? (read) 'y)
                    (printrow (printn s 10))
                    'done))])
      (printrow (printn s 10)))))


