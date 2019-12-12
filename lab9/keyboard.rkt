#lang racket
(require "streams.rkt")
(provide keyboard-stream output$ empty-stream?)

(define keyboard-stream                 ; (keyboard-stream) is a stream
  (lambda ()                            ; of s-expressions typed by the
    (display "? ")                      ; user
    (let ((this (read)))
      (if (eof-object? this)
	  'the-empty-stream
	  (cons$ this (keyboard-stream))))))

; The body of a promise has not been evaluated yet

(define output$
  (lambda (s)
    (cond
     ((empty-stream? s) 'done)
     (else
      (display (car$ s))
      (newline) 
      (output$ (cdr$ s))))))

(define empty-stream?
  (lambda (s)
    (eq? s 'the-empty-stream)))