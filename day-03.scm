#lang racket

(define (next-pos pos cmd)
  (cond ((equal? cmd #\^) (cons (add1 (car pos)) (cdr pos)))
        ((equal? cmd #\v) (cons (sub1 (car pos)) (cdr pos)))
        ((equal? cmd #\<) (cons (car pos) (sub1 (cdr pos))))
        ((equal? cmd #\>) (cons (car pos) (add1 (cdr pos))))))

(define (assoc-or key alist fallback)
  (let ((res (assoc key alist)))
    (if res res (cons key fallback))))

(define (init)
  (let ((state '(((0 . 0) . 1))))
    (lambda (cmd)
      (let* ((pos (next-pos (caar state) cmd))
             (old-count (cdr (assoc-or pos state 0))))
        (set! state (cons (cons pos (add1 old-count)) state))
        state))))