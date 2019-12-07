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

(define (path-to-center orbit-map object)
  (if (equal? object COM) '()
      (cons object (path-to-center orbit-map (object-orbited object orbit-map)))))
(define (number-of-orbits orbit-map object)
  (length (path-to-center orbit-map object)))

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

(begin
  (display
    (foldl + 0
           (map (lambda (object) (number-of-orbits orbit-map object)) objects)))
  (newline))

(define (distance-to-target orbit-map start target)
  (let ((target-to-center (path-to-center orbit-map target))
        (start-to-center (path-to-center orbit-map start)))
    (let loop ((path-to-start (reverse start-to-center))
               (path-to-target (reverse target-to-center)))
      (cond
        ((and (null? path-to-start)
              (null? path-to-target))
         0)

        ((null? path-to-start)
         (+ 1 (loop '() (cdr path-to-target))))
        ((null? path-to-target)
         (+ 1 (loop (cdr path-to-start) '())))

        ((equal? (car path-to-start) (car path-to-target))
         (loop (cdr path-to-start) (cdr path-to-target)))

        (else (+ 2 (loop (cdr path-to-target) (cdr path-to-start))))))))

(begin
  (display
    ; - 2 because we don't want to orbit Santa, just the thing he's orbiting.
    (- (distance-to-target orbit-map "YOU" "SAN") 2))
  (newline))
