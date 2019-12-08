(import intcode
        (chicken process-context))

(define (main args)
  (if (null? args)
      (print "Please provide the name of a program file to load.")
      (let* ((program (read-program-from-file (car args)))
             (computer (make-intcode-machine program)))
        (computer))))

(main (command-line-arguments))
