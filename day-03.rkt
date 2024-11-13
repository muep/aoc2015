#lang racket

(require srfi/1)
(require srfi/41)

(provide part-1)
(provide part-2)

(define (slurp path)
  (let ((port (open-input-file path)))
    (port->string port #:close? #t)))

(define (next-pos pos cmd)
  (cond ((equal? cmd #\^) (cons (add1 (car pos)) (cdr pos)))
        ((equal? cmd #\v) (cons (sub1 (car pos)) (cdr pos)))
        ((equal? cmd #\<) (cons (car pos) (sub1 (cdr pos))))
        ((equal? cmd #\>) (cons (car pos) (add1 (cdr pos))))))

(define (assoc-or key alist fallback)
  (let ((res (assoc key alist)))
    (if res res (cons key fallback))))

(define (cleanup state)
  (delete-duplicates state (lambda (a b)
                             (equal? (car a) (car b)))))

(define (init)
  (let ((state '(((0 . 0) . 1))))
    (lambda (cmd)
      (let* ((pos (next-pos (caar state) cmd))
             (old-count (cdr (assoc-or pos state 0))))
        (when (pair? pos)
          (set! state (cons (cons pos (add1 old-count)) state)))
        state))))

(define (process input)
  (let ((step (init)))
    (for/last ((cmd input))
      (step cmd))))

(define (part-1 path)
  (let ((input (slurp path)))
    (length (cleanup (process input)))))

(define (init-2)
  (let ((next-santa (init))
        (other-santa (init))
        ;; This last one we just know of the implementation
        (previous-state '(((0 . 0) . 1))))
    (lambda (cmd)
      (let* ((state (next-santa cmd))
             (tmp-santa other-santa)
             (tmp-state previous-state))
        (set! other-santa next-santa)
        (set! next-santa tmp-santa)
        (set! previous-state state)
        (list state tmp-state)))))

(define (process-2 input)
  (let ((step (init-2)))
    (for/last ((cmd input))
      (step cmd))))

(define (part-2 path)
  (let ((input (slurp path)))
    (length (cleanup (apply append (process-2 input))))))