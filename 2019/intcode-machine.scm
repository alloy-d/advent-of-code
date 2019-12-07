(import (chicken io)
        (chicken string))


(define (read-input)
  (map string->number
       (string-split (string-chomp (read-string)) ",")))

(define pos 0)
(define initial-state '())

(define (load! input-file)
  (set! initial-state
    (with-input-from-file input-file read-input))
  (reset!))

(define (reset!)
  (set! program (list->vector initial-state))
  (set! pos 0))
(reset!)

(define (debug)
  (display "pos: ")
  (display pos)
  (newline)
  (display program)
  (newline))

(define (get-at pos)
  (vector-ref program pos))
(define (get-reffed ref-pos)
  (get-at (get-at ref-pos)))

(define position get-reffed)
(define immediate get-at)

(define (set-at! pos val)
  (vector-set! program pos val))
(define (set-reffed! pos val)
  (set-at! (get-at pos) val))

(define (advance! amount)
  (set! pos (+ amount pos)))

(define (do-math! op get-0 get-1)
  (let* ((a (get-0 (+ 1 pos)))
         (b (get-1 (+ 2 pos)))
         (result (op a b)))
    (set-reffed! (+ 3 pos) result)))

(define (opcode-part instruction)
  (remainder instruction 100))
(define (modes-part instruction)
  (floor (/ instruction 100)))

; returns the mode of the parameter at index, given modes.
; indices are 0-based.
(define (mode modes index)
  (remainder (floor (/ modes (expt 10 index)))
             10))
(define (mode-proc modes index)
  (if (= 0 (mode modes index))
      position
      immediate))
(define (mode-procs modes count)
  (let loop ((index 0))
    (if (= index count) '()
        (cons (mode-proc modes index)
              (loop (+ index 1))))))

(define (handle-halt) '())
(define (handle-add! get-0 get-1)
  (do-math! + get-0 get-1))
(define (handle-multiply! get-0 get-1)
  (do-math! * get-0 get-1))
(define (handle-store-input!)
  (display "INPUT: " (current-error-port))
  (set-reffed! (+ pos 1) (read)))
(define (handle-output get-0)
  (display "OUTPUT: " (current-error-port))
  (display (get-0 (+ pos 1)))
  (newline))

(define (handle-jump-if! test)
  (lambda (get-0 get-1)
    (if (test (get-0 (+ pos 1)))
        (set! pos (get-1 (+ pos 2)))
        (advance! 3))))
(define handle-jump-if-true!
  (handle-jump-if! (compose not zero?)))
(define handle-jump-if-false!
  (handle-jump-if! zero?))

(define (handle-cmp! test)
  (lambda (get-0 get-1)
    (set-reffed! (+ pos 3)
                 (if (test (get-0 (+ pos 1))
                           (get-1 (+ pos 2)))
                     1
                     0))))
(define handle-less-than!
  (handle-cmp! <))
(define handle-equals!
  (handle-cmp! =))

(define (handle!)
  (let* ((instruction (get-at pos))
         (opcode (opcode-part instruction))
         (modes (modes-part instruction)))
    ;(debug)
    (cond
      ((= opcode 99) (handle-halt))

      ((= opcode 1) (apply handle-add! (mode-procs modes 2))
                    (advance! 4)
                    (handle!))

      ((= opcode 2) (apply handle-multiply! (mode-procs modes 2))
                    (advance! 4)
                    (handle!))

      ((= opcode 3) (handle-store-input!)
                    (advance! 2)
                    (handle!))

      ((= opcode 4) (apply handle-output (mode-procs modes 1))
                    (advance! 2)
                    (handle!))

      ((= opcode 5) (apply handle-jump-if-true! (mode-procs modes 2))
                    (handle!))

      ((= opcode 6) (apply handle-jump-if-false! (mode-procs modes 2))
                    (handle!))

      ((= opcode 7) (apply handle-less-than! (mode-procs modes 2))
                    (advance! 4)
                    (handle!))

      ((= opcode 8) (apply handle-equals! (mode-procs modes 2))
                    (advance! 4)
                    (handle!))
      )))

(define (seq start stop)
  (let loop ((value start)
             (result '()))
    (if (> value stop)
        result
        (loop (+ value 1) (cons value result)))))

(define (empty? list)
  (eqv? list '()))

(define (main args)
  (if (null? args)
      (display "Please provide the name of a program file to load.")
      (begin
        (load! (car args))
        (handle!))))
