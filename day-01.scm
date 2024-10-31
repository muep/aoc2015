(define day-01
  (let* ((foldl (lambda (f init seq-in)
                  (let loop ((acc init)
                             (seq seq-in))
                    (if (null? seq)
                        acc
                        (loop (f (car seq) acc) (cdr seq))))))
         (instruction->offset (lambda (instruction)
                                (cond
                                 ((equal? #\( instruction) 1)
                                 ((equal? #\) instruction) -1)
                                 (else 0))))
         (part-1 (lambda (lines)
                   (foldl + 0 (map instruction->offset
                                   (string->list 
                                    (apply string-append lines))))))
         (part-2 (lambda (lines)
                   (car
                    (foldl
                     (lambda (offset state)
                       (let ((pos (car state))
                             (sum (cdr state)))
                         (if (negative? sum)
                             state
                             (cons (+ 1 pos) (+ sum offset)))))
                     '(0 . 0)
                     (map instruction->offset
                          (string->list 
                           (apply string-append lines))))))))
    `((part-1 . ,part-1)
      (part-2 . ,part-2))))
