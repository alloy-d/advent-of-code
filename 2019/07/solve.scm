(load "../intcode.so")
(import (chicken io)
        (chicken port)
        (chicken process)
        intcode)

(define (print-error . args)
  (with-output-to-port (current-error-port)
                       (lambda () (apply print args))))

(define input-program (read-program-from-file "input"))

(define (make-amplifier)
  (make-intcode-machine input-program))

(define (get-amplifier-signal phase-setting input-value)
  (let-values
    (((amplifier input output halt) (make-amplifier))
     ((output-value) (make-parameter '())))

    (input (lambda ()
             (input (constantly input-value))
             phase-setting))
    (output output-value)

    (amplifier)
    (output-value)))

(define (get-amplifier-sequence-signal sequence)
  (let next ((phase-settings sequence)
             (input 0))
    (print* sequence " " input "\r")

    (if (null? phase-settings) input
        (next (cdr phase-settings)
              (get-amplifier-signal (car phase-settings) input)))))

(define (get-amplifier-loop-signal phases)
  (let*-values
    (
     ((_) (print-error "STARTING DEFINITIONS"))
     ((a a-in a-out a-halt) (make-amplifier))
     ((b b-in b-out b-halt) (make-amplifier))
     ((c c-in c-out c-halt) (make-amplifier))
     ((d d-in d-out d-halt) (make-amplifier))
     ((e e-in e-out e-halt) (make-amplifier))
     ((a-phase b-phase c-phase d-phase e-phase) (apply values phases))
     ((a-resume b-resume c-resume d-resume e-resume)
      (apply values
             (map (lambda (f) (make-parameter (lambda (_) (f))))
                  (list a b c d e)))))

    (define (input-phase-then-resume-setup phase set-input-proc set-resume-proc start)
      (call-with-current-continuation
        (lambda (resume-setup)
          (set-input-proc (lambda ()
                            ;; first call: change input function to continue setup
                            ;; next time it's called.
                            (set-input-proc (lambda ()
                                              ;; next call: save continuation of input
                                              ;; function and return to the setup function.

                                              ;; Continuation will need to be called with next
                                              ;; input value.
                                              (call-with-current-continuation
                                                (lambda (input)
                                                  (set-resume-proc input)
                                                  (resume-setup '())))))
                            ;; then return the phase setting.
                            phase))
          (start))))

    (input-phase-then-resume-setup a-phase a-in a-resume a)
    (input-phase-then-resume-setup b-phase b-in b-resume b)
    (input-phase-then-resume-setup c-phase c-in c-resume c)
    (input-phase-then-resume-setup d-phase d-in d-resume d)
    (input-phase-then-resume-setup e-phase e-in e-resume e)

    (define (connect-io current-name
                        set-resume-current
                        set-current-input-proc
                        set-current-output-proc
                        send-output-to-next)
      (set-current-output-proc (lambda (value)
                                 (print-error "SENDING OUTPUT VALUE: " value)
                                 (let ((next-input-value
                                         ; save this continuation to our resume function.
                                         ; it will be called when the previous amp calls an
                                         ; output function, so it will receive the next input
                                         ; value.
                                         (call-with-current-continuation
                                           (lambda (resume-from-output)
                                             (set-resume-current resume-from-output)
                                             (print-error "PAUSING ON OUTPUT: " current-name)
                                             ((send-output-to-next) value)))))
                                   (print-error "RESUMING ON INPUT: " current-name)
                                   (set-current-input-proc (constantly next-input-value))))))

    (connect-io "A" a-resume a-in a-out b-resume)
    (connect-io "B" b-resume b-in b-out c-resume)
    (connect-io "C" c-resume c-in c-out d-resume)
    (connect-io "D" d-resume d-in d-out e-resume)
    (connect-io "E" e-resume e-in e-out a-resume)

    (define (after-halting name set-halt-proc get-continue-proc)
      (set-halt-proc (lambda ()
                       (print-error "HALTING: " name)
                       ((get-continue-proc) '()))))

    (after-halting "A" a-halt b-resume)
    (after-halting "B" b-halt c-resume)
    (after-halting "C" c-halt d-resume)
    (after-halting "D" d-halt e-resume)
    (call-with-current-continuation
      (lambda (return)
        ; Return here once the final amp has halted.
        (after-halting "E" e-halt (constantly return))

        ; Now start the whole thing by sending the initial input to A.
        ((a-resume) 0)))


    ; The final signal to the thrusters is also wired to A's input.
    ((a-in))))

(define (remove-once number numbers)
  (cond
    ((null? numbers)
     '())
    ((= number (car numbers))
     (cdr numbers))
    (else
      (cons (car numbers)
            (remove-once number (cdr numbers))))))

(define (permute numbers)
  (let* ((with-first-element
           (lambda (element)
             (lambda (some-list)
               (cons element some-list))))
         (permutations-with-x-first
           (lambda (numbers)
             (lambda (x)
               (map (with-first-element x)
                    (permute (remove-once x numbers)))))))

    (if (= (length numbers) 1) (list numbers)
        (apply append
               (map (permutations-with-x-first numbers) numbers)))))

(print "\rMaximum signal: "
  (let find-best ((candidates (permute '(0 1 2 3 4)))
                  (best-so-far -1))
    (if (null? candidates) best-so-far
        (let ((current-signal (get-amplifier-sequence-signal (car candidates))))
          (find-best (cdr candidates)
            (max current-signal best-so-far))))))

(print "\rMaximum loop signal: "
  (let find-best ((candidates (permute '(5 6 7 8 9)))
                  (best-so-far -1))
    (if (null? candidates) best-so-far
        (let ((current-signal (get-amplifier-loop-signal (car candidates))))
          (print* (car candidates) " " best-so-far "\r")
          (find-best (cdr candidates)
            (max current-signal best-so-far))))))
