#lang racket

(require srfi/13)
(require (prefix-in day-01:: "day-01.rkt"))
(require (prefix-in day-02:: "day-02.rkt"))
(require (prefix-in day-03:: "day-03.rkt"))

(define days
  `((1 . ((1 . ,day-01::part-1)
          (2 . ,day-01::part-2)))
    (2 . ((1 . ,day-02::part-1)
          (2 . ,day-02::part-2)))
    (3 . ((1 . ,day-03::part-1)
          (2 . ,day-03::part-2)))))

(define (run-parts parts path)
  (let ((part-1 (assoc 1 parts))
        (part-2 (assoc 2 parts)))
    (when part-1
      (display "  part-1: ")
      (display ((cdr part-1) path))
      (newline))
    (when part-2
      (display "  part-2: ")
      (display ((cdr part-2) path))
      (newline))))

(define (run-day daynum)
  (let ((day-entry (assoc daynum days)))
    (if day-entry
        (begin
          (display (string-append "day " (number->string daynum) ":\n"))
          (run-parts (cdr day-entry)
                     (string-append "input/day-"
                                    (string-pad (number->string daynum) 2 #\0)
                                    ".txt")))
        (display (string-append "No solution for day " daynum "\n")))))

(define (main)
  (for-each run-day (map car days)))

(main)