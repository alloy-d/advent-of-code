(module intcode (read-program read-program-from-file make-intcode-machine)
  (import scheme
          (chicken base)
          (chicken bitwise)
          (chicken io)
          (chicken string))

  ; like (define x func), except works in a letrec when func is also
  ; declared in the letrec and its value is not yet available
  (define-syntax wrapper
    (syntax-rules ()
      ((wrapper func)
       (lambda args (apply func args)))))

  (define (read-program)
    (list->vector
      (map string->number
           (string-split (string-chomp (read-string)) ","))))

  (define (read-program-from-file input-file)
    (with-input-from-file input-file read-program))

  (define (opcode-part instruction)
    (remainder instruction 100))
  (define (modes-part instruction)
    (floor (/ instruction 100)))

  ; in: a binary-looking number in decimal form, like 1001
  ; out: a number that has that binary form, like 9
  (define (decbin->bin num)
    (if (= num 0) 0
        (+ (arithmetic-shift (decbin->bin (floor (/ num 10))) 1)
           (remainder num 10))))

  ; returns the mode of the parameter at index, given modes.
  ; indices are 0-based.
  (define (mode modes index)
    (bitwise-and 1 (arithmetic-shift modes (- index))))

  (define (make-intcode-machine program)
    (letrec ((pos (make-parameter 0))
             (advance!
               (lambda (count)
                 (pos (+ count (pos)))))

             (get-at
               (lambda (pos)
                 (vector-ref program pos)))
             (get-reffed
               (lambda (ref-pos)
                 (get-at (get-at ref-pos))))

             (set-at!
               (lambda (pos val)
                 (vector-set! program pos val)))
             (set-reffed!
               (lambda (ref-pos val)
                 (set-at! (get-at ref-pos) val)))

             ; parameter modes
             (position (wrapper get-reffed))
             (immediate (wrapper get-at))
             (mode-proc
               (lambda (modes index)
                 (if (= 0 (mode modes index))
                     position
                     immediate)))

             (args
               (lambda (count modes)
                 (let loop ((index 0))
                   (if (= index count) '()
                       (cons ((mode-proc modes index) (+ (pos) index 1))
                             (loop (+ index 1)))))))

             ; reusable operation archetypes
             (operation-math
               (lambda (operation)
                 (lambda (a b dest)
                   (set-at! dest (operation a b)))))
             (operation-jump-if
               (lambda (test)
                 (lambda (arg dest)
                   (if (test arg)
                       (pos dest)
                       ; TODO: there's probably a more interesting way to
                       ; do this conditional advancement
                       (advance! 3)))))
             (operation-compare
               (lambda (cmp)
                 (lambda (a b dest)
                   (let ((result (if (cmp a b) 1 0)))
                     (set-at! dest result)))))

             ; the operations themselves
             (do-add! (wrapper (operation-math +)))
             (do-multiply! (wrapper (operation-math *)))
             (do-jump-if-true! (wrapper (operation-jump-if (compose not zero?))))
             (do-jump-if-false! (wrapper (operation-jump-if zero?)))
             (do-less-than! (wrapper (operation-compare <)))
             (do-equals! (wrapper (operation-compare =)))
             (do-store-input!
               (lambda (dest)
                 (display "INPUT: " (current-error-port))
                 (set-at! dest (read))))
             (do-output
               (lambda (val)
                 (display "OUTPUT: " (current-error-port))
                 (display val)
                 (newline)))
             (do-halt (lambda () '())))

      (letrec ((operation-mapping
                 ; format: (opcode func num-args mode-mask advance? continue?)
                 `((1  ,do-add!            3 #b100 #t #t)
                   (2  ,do-multiply!       3 #b100 #t #t)
                   (3  ,do-store-input!    1 #b1   #t #t)
                   (4  ,do-output          1 #b0   #t #t)
                   (5  ,do-jump-if-true!   2 #b00  #f #t)
                   (6  ,do-jump-if-false!  2 #b00  #f #t)
                   (7  ,do-less-than!      3 #b100 #t #t)
                   (8  ,do-equals!         3 #b100 #t #t)
                   (99 ,do-halt            0 #b0   #f #f)))

               (debug
                 (lambda ()
                   (print "POSITION: " (pos) " PROGRAM: " program)))

               (handle!
                 (lambda ()
                   ;(debug)
                   (let* ((instruction (get-at (pos)))
                          (opcode (opcode-part instruction))
                          (modes (decbin->bin (modes-part instruction))))
                     (let-values (((func num-args mode-mask advance? continue?)
                                   (apply values (cdr (assq opcode operation-mapping)))))
                       (let* ((modes (bitwise-ior modes mode-mask))
                              (args (args num-args modes)))
                         ;(print "OPCODE: " opcode
                         ;       " MODES: " modes
                         ;       " FUNC: " func
                         ;       " ARGS: " args)
                         (apply func args))
                       (when advance? (advance! (+ 1 num-args)))
                       (when continue? (handle!)))))))
        handle!))))
