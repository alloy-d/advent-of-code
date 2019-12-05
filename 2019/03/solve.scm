(import (chicken io)
        (chicken string)
        srfi-113
        srfi-128)

(define (empty? l)
  (eqv? l '()))

(define (parse-movement str)
  (let* ((chars (string->list str))
         (dir (car chars))
         (dist (string->number (list->string (cdr chars))))

         (x-movement (* dist
                        (cond ((eqv? dir #\R)  1)
                              ((eqv? dir #\L) -1)
                              (#t              0))))
         (y-movement (* dist
                        (cond ((eqv? dir #\U)  1)
                              ((eqv? dir #\D) -1)
                              (#t              0)))))
    (cons x-movement y-movement)))

(define space-comparator
  (let ((coord-comparator
          (make-comparator integer? = < number-hash)))
    (make-pair-comparator coord-comparator coord-comparator)))

(define (make-space-set)
  (set space-comparator))

(define (add-coords coord1 coord2)
  (cons (+ (car coord1) (car coord2))
        (+ (cdr coord1) (cdr coord2))))

(define (next-int-toward-zero n)
  (cond ((= n 0) 0)
        ((> n 0) (- n 1))
        ((< n 0) (+ n 1))))

(define (next-smaller-movement movement)
  (cons (next-int-toward-zero (car movement))
        (next-int-toward-zero (cdr movement))))

(define (spaces-covered-by-movement start movement)
  (let loop ((spaces (make-space-set))
             (movement movement))
    (if (equal? movement '(0 . 0))
        spaces
        (let ((current-space (add-coords start movement)))
          (loop (set-adjoin spaces current-space)
                (next-smaller-movement movement))))))

(define (spaces-covered-by-movements movements)
  (let account-for-movements ((movements movements)
                              (pos '(0 . 0))
                              (spaces-covered-previously (make-space-set)))
    (if (empty? movements)
        spaces-covered-previously
        (let ((current-movement (car movements))
              (remaining-movements (cdr movements)))
          (account-for-movements
            remaining-movements
            (add-coords current-movement pos)
            (set-union (spaces-covered-by-movement pos current-movement)
                       spaces-covered-previously))))))

; Here, a "wire" is a list of movements.
(define (find-intersections wires)
  (let ((spaces-covered-per-wire (map spaces-covered-by-movements wires)))
    (apply set-intersection spaces-covered-per-wire)))

(define (manhattan-distance-from-origin coords)
  (+ (abs (car coords)) (abs (cdr coords))))

(define (parse-input-line line)
  (map parse-movement (string-split line ",")))

(define (read-input-lines)
  (let ((line (read-line)))
    (if (eqv? line #!eof)
        '()
        (cons (parse-input-line line) (read-input-lines)))))

(define input
  (with-input-from-file "input" read-input-lines))

(define (main args)
  (display "manhattan distance to nearest intersection: ")
  (display
    (apply min (map manhattan-distance-from-origin
                    (set->list (find-intersections input))))))
