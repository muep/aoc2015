(define day-03
  (let* ((next-pos (lambda (pos cmd)
                     (cond ((equal? cmd #\^) (cons (+ (car pos) 1) (cdr pos)))
                           ((equal? cmd #\v) (cons (- (car pos) 1) (cdr pos)))
                           ((equal? cmd #\<) (cons (car pos) (- (cdr pos) 1)))
                           ((equal? cmd #\>) (cons (car pos) (+ (cdr pos) 1))))))
         (assoc-or (lambda (key alist fallback)
                     (let ((res (assoc key alist)))
                       (if res res (cons key fallback)))))
         (init (lambda ()
                 (let ((state '(((0 . 0) . 1))))
                   (lambda (cmd)
                     (let* ((pos (next-pos (caar state) cmd))
                            (old-count (cdr (assoc-or pos state 0))))
                       (set! state (cons (cons pos (+ 1 old-count)) state))
                       state)))))
         (part-1 (lambda (lines)
                   #f)))
    `((init . ,init))))
