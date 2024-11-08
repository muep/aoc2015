#lang racket

(define (map-lines- process-line port acc)
  (let ((line (read-line port)))
      (if (eof-object? line)
          (reverse acc)
          (map-lines- process-line port (cons (process-line line) acc)))))

(define (map-lines process-line port)
  (map-lines- process-line port '()))

(define (giftbox-prop f)
  (lambda (giftbox-line)
    (let* ((pieces (map string->number (string-split giftbox-line "x")))
           (l (first pieces))
           (w (second pieces))
           (h (third pieces)))
      (f l w h))))

(define (paper-amount l w h)
  (+ (min (* l w) (* l h) (* w h))
       (* 2 l w) (* 2 l h) (* 2 w h)))

(define (ribbon-amount l w h)
  (+ (* 2 (min (+ l w) (+ l h) (+ w h))) (* l w h)))

(define giftbox->area (giftbox-prop paper-amount))
(define giftbox->ribbon-amount (giftbox-prop ribbon-amount))

(define (day-02-part-1 filename)
  (let ((f (open-input-file filename)))
    (foldl + 0 (map-lines giftbox->area f))))

(define (day-02-part-2 filename)
  (let ((f (open-input-file filename)))
    (foldl + 0 (map-lines giftbox->ribbon-amount f))))