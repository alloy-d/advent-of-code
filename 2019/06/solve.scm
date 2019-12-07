(import (chicken io)
        (chicken string))

(define COM "COM")

; returns orbit-map with the addition of orbit-pair.
; orbit pair has the format (orbited orbiting).
(define (register-orbit orbit-map orbit-pair)
  (let ((orbited (car orbit-pair))
        (orbiting (cadr orbit-pair)))
    (cons (list orbiting orbited) orbit-map)))

; (object-orbited "A" orbit-map) => whatever A is orbiting
(define (object-orbited object orbit-map)
  (cadr (assoc object orbit-map)))

(define (number-of-orbits orbit-map object)
  (if (equal? object COM) 0
      (+ 1 (number-of-orbits orbit-map (object-orbited object orbit-map)))))

(define (parse-line line)
  (string-split line ")"))
(define (read-input)
  (let loop ((line (read-line)))
    (if (eq? line #!eof) '()
        (cons (parse-line line) (loop (read-line))))))

(define orbit-map
  (foldl register-orbit '()
         (with-input-from-file "input" read-input)))

(define objects
  (map car orbit-map))

(display
  (foldl + 0
         (map (lambda (object) (number-of-orbits orbit-map object)) objects)))
