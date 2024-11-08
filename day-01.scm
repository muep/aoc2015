#lang racket

(define (slurp path)
  (let ((port (open-input-file path)))
    (port->string port #:close? #t)))

(define (instruction->offset instruction)
  (cond
    ((equal? #\( instruction) 1)
    ((equal? #\) instruction) -1)
    (else 0)))

(define (day-01-part-1 path)
  (foldl + 0 (map instruction->offset (string->list (slurp path)))))

(define (day-01-part-2 path)
  (car
   (foldl
    (lambda (offset state)
      (let ((pos (car state))
            (sum (cdr state)))
        (if (negative? sum)
            state
            (cons (+ 1 pos) (+ sum offset)))))
    '(0 . 0)
    (map instruction->offset (string->list (slurp path))))))