(import (chicken io)
        (chicken string))

(define (read-input)
  (map string->number
       (string-split (string-chomp (read-string)) ",")))

(define pos 0)
(define initial-state (read-input))

(define (reset!)
  (set! program (list->vector initial-state))
  (set! pos 0))
(reset!)


(define (get-at pos)
  (vector-ref program pos))
(define (get-reffed ref-pos)
  (get-at (get-at ref-pos)))

(define (set-at! pos val)
  (vector-set! program pos val))
(define (set-reffed! pos val)
  (set-at! (get-at pos) val))

(define (advance!)
  (set! pos (+ 4 pos)))

(define (do-math! op)
  (let* ((a (get-reffed (+ 1 pos)))
         (b (get-reffed (+ 2 pos)))
         (result (op a b)))
    (set-reffed! (+ 3 pos) result)))

(define (handle-halt) '())
(define (handle-add!)
  (do-math! +))
(define (handle-multiply!)
  (do-math! *))
(define (handle!)
  (let ((opcode (get-at pos)))
    ;(newline)
    ;(display pos)
    ;(display program)
    (cond
      ((= opcode 99) (handle-halt))
      ((= opcode 1) (begin
                      (handle-add!)
                      (advance!)
                      (handle!)))
      ((= opcode 2) (begin
                      (handle-multiply!)
                      (advance!)
                      (handle!))))))

(define (prepare! noun verb)
  (set-at! 1 noun)
  (set-at! 2 verb))
(define (get-final-pos-0 noun verb)
  (reset!)
  (prepare! noun verb)
  (handle!)
  (get-at 0))

(define (discover-alarm-value)
  (get-final-pos-0 12 2))

(define (seq start stop)
  (let loop ((value start)
             (result '()))
    (if (> value stop)
        result
        (loop (+ value 1) (cons value result)))))

(define (empty? list)
  (eqv? list '()))

(define (produce-test-values upper-limit)
  (let ((vals (seq 0 upper-limit)))
    (let loop ((noun-vals vals)
               (verb-vals vals)
               (produced '()))
      (cond
        ((empty? verb-vals)
         produced)

        ((empty? noun-vals)
         (loop vals (cdr verb-vals) produced))

        (#t
         (let ((pair (cons (car noun-vals) (car verb-vals))))
           (loop (cdr noun-vals) verb-vals
                 (cons pair produced))))))))

;; In reality, I just ran this with (produce-test-values 5) and then saw
;; the relationship and mathed it out.
(define (find-desired-value value)
  (do ((test-values (produce-test-values 500) (cdr test-values)))
    ((= value (get-final-pos-0 42 (cdr (car test-values))))
     (newline)
     (display "got it! ")
     (display 42)
     (display " ")
     (display (cdr (car test-values))))

    (newline)
    (display 42)
    (display " ")
    (display (cdr (car test-values)))
    (display " ")
    (display (get-at 0))))

(define (main args)
  (let ((part-a-solution (discover-alarm-value)))
    (newline)
    (display "part a solution: ")
    (display part-a-solution)

    (find-desired-value 19690720)))
