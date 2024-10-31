(define (load-input path)
  (let ((f (open-input-file path)))
    (let loop ((buf '()))
      (let ((line (read-line f)))
        (if (eof-object? line)
            (begin
              (close-input-port f)
              (reverse buf))
            (loop (cons line buf)))))))

(define (run-parts input parts n)
  (unless (null? parts)
    (display (string-append "  Part " (number->string n) ": "))
    (display (number->string ((car parts) input)))
    (newline)
    (run-parts input (cdr parts) (+ n 1))))

(define (get-parts day)
  (list (cdr (assoc 'part-1 day))
        (cdr (assoc 'part-2 day))))

(display "Day 01\n")
(load "day-01.scm")
(define day-01-input (load-input "input/day-01.txt"))
(run-parts day-01-input (get-parts day-01) 1)

(load "day-02.scm")
(define day-02-input (load-input "input/day-02.txt"))
(run-parts day-02-input (get-parts day-02) 1)
