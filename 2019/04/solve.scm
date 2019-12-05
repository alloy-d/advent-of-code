(define input-range '(206928 . 679128))

(define (intchar->integer char)
  (- (char->integer char) 48)) ; where 48 is (char->integer #\0)

(define (integer->list k)
  (map intchar->integer
        ((compose string->list number->string) k)))

(define (empty? l)
  (eqv? l '()))

(define (contains-double? things)
  (let check ((last '())
              (current-run-length 0)
              (things things))
    (let ((last-was-double (= current-run-length 1)))
      (cond ((empty? things) last-was-double)
            ((eqv? last (car things))
             (check (car things) (+ current-run-length 1) (cdr things)))
            (#t
             (if last-was-double #t
                 (check (car things) 0 (cdr things))))))))

(define (satisfactory? number)
  (let ((intlist (integer->list number)))
    (and (apply <= intlist)
         (contains-double? intlist))))

(define (seq start stop)
  (if (> start stop) '()
      (cons start (seq (+ 1 start) stop))))

(define (1-if-true bool)
  (if bool 1 0))

(foldl + 0 (map (compose 1-if-true satisfactory?) (seq (car input-range) (cdr input-range))))
