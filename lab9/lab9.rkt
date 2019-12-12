#lang racket

(require "streams.rkt")
(require "keyboard.rkt")

(define rember-all$ (lambda (x s)
                      (cond
                        [(eq? (car$ s) x) (rember-all$ x (cdr$ s))]
                        [else (cons$ (car$ s) (rember-all$ x (cdr$ s)))])))

(define subst-all$ (lambda (x y s)
                     (cond
                       [(eq? (car$ s) x) (cons$ y (subst-all$ x y (cdr$ s)))]
                       [else (cons$ (car$ s) (subst-all$ x y (cdr$ s)))])))

(define nextPair (lambda (p)
                   (cond
                     [(= (cdr p) 1) (cons 1 (+ 1 (car p)))]
                     [else (cons (+ 1 (car p)) (- (cdr p) 1))])))

(define pairsFrom$  (lambda (p)
                      (cons$ p (pairsFrom$ (nextPair p)))))

(define allPairs$ (pairsFrom$ (cons 1 1)))

(define merge$ (lambda (s1 s2)
                 (cond
                   [(= (car$ s1) (car$ s2)) (merge$ s1 (cdr$ s2))]
                   [(< (car$ s1) (car$ s2)) (cons$ (car$ s1) (merge$ (cdr$ s1) s2))]
                   [else (cons$ (car$ s2) (merge$ s1 (cdr$ s2)))])))

(define scale$ (lambda (s factor)
                 (map$ (lambda (num) (* num factor)) s)))

(define Ham$ (cons$ 1 (merge$ (scale$ Ham$ 2) (merge$ (scale$ Ham$ 3) (scale$ Ham$ 5)))))

(define *$ (lambda (s1 s2)
             (cons$ (* (car$ s1) (car$ s2)) (*$ (cdr$ s1) (cdr$ s2)))))

; (define fact-stream$ (cons$ 1 (*$ fact-stream$ (IntsFrom$ 1)))) 

(define Twos$ (cons$ 2 Twos$))

(define odd-stream$ (cons$ 3 (+$ odd-stream$ Twos$)))

(define even-stream$ (cons$ 2 (+$ even-stream$ Twos$)))

(define odd-fact-stream$ (cons$ 1 (*$ odd-fact-stream$ (*$ even-stream$ odd-stream$))))

(define even-fact-stream$ (cons$ 1 (*$ odd-fact-stream$ even-stream$)))

(define _helper$ (lambda (s isMinus)
                     (let
                         ([denom (if isMinus (- 0 (car$ s)) (car$ s))])
                       (cons$ (/ 1.0 denom) (cons$ 0 (_helper$ (cdr$ s) (not isMinus)))))))

(define sin-coeffs$ (cons$ 0 (_helper$ odd-fact-stream$ #f)))

(define cos-coeffs$ (_helper$ even-fact-stream$ #f))

(define cadr$ (lambda (s)
                (car$ (cdr$ s))))

(define grune-a-b (lambda (s)
                    (if (and (eq? (car$ s) 'a) (eq? (cadr$ s) 'a))
                        (cons$ 'b (grune-a-b (cdr$ (cdr$ s))))
                        (cons$ (car$ s) (grune-a-b (cdr$ s))))))

(define grune (lambda (a b) (lambda (s)
                    (if (and (eq? (car$ s) a) (eq? (cadr$ s) a))
                        (cons$ b ((grune a b) (cdr$ (cdr$ s))))
                        (cons$ (car$ s) ((grune a b) (cdr$ s)))))))

(define Tester$ (cons$ 1 (cons$ 2 (cons$ 3 Tester$))))