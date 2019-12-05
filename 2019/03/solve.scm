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

(define integer-comparator
  (make-comparator integer? = < number-hash))
(define space-comparator
  (make-pair-comparator integer-comparator integer-comparator))
(define space-with-distance-comparator
  (make-pair-comparator
    space-comparator
    (make-comparator integer?
                     (lambda (x y) #t) ; we just care that it exists
                     (lambda (x y) #t)
                     (lambda (x) 0))))

(define (make-space-set)
  (set space-with-distance-comparator))

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

(define space-with-distance cons)
(define (spaces-covered-by-movement start movement)
  (let loop ((spaces (make-space-set))
             (movement movement)
             (distance-traveled
               (+ (cdr start)
                  (manhattan-distance-from-origin movement))))
    (if (equal? movement '(0 . 0))
        spaces
        (let ((current-space (space-with-distance
                               (add-coords (car start) movement)
                               distance-traveled)))
          (loop (set-adjoin spaces current-space)
                (next-smaller-movement movement)
                (- distance-traveled 1))))))

(define (manhattan-distance-from-origin coords)
  (+ (abs (car coords)) (abs (cdr coords))))

(define (spaces-covered-by-movements movements)
  (let account-for-movements ((movements movements)
                              (pos (space-with-distance '(0 . 0) 0))
                              (spaces-covered-previously (make-space-set)))
    (if (empty? movements)
        spaces-covered-previously
        (let ((current-movement (car movements))
              (remaining-movements (cdr movements)))
          (account-for-movements
            remaining-movements
            (space-with-distance
              (add-coords current-movement (car pos))
              (+ (manhattan-distance-from-origin current-movement)
                 (cdr pos)))
            (set-union spaces-covered-previously
                       (spaces-covered-by-movement pos current-movement)))))))

; Here, a "wire" is a list of movements.
(define (spaces-covered-per-wire wires)
  (map spaces-covered-by-movements wires))

(define (find-intersections spaces-covered-per-wire)
  (apply set-intersection spaces-covered-per-wire))

(define (distance-to-space space spaces-covered)
  (cdr (set-member spaces-covered
                   space
                   (space-with-distance '(0 . 0) +inf.0))))

(define (combined-distance-to-space space coverages)
  (apply + (map (lambda (spaces-covered)
                  (distance-to-space space spaces-covered))
                coverages)))

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
  (let* ((coverages (spaces-covered-per-wire input))
         (intersections (find-intersections coverages)))
    (display "manhattan distance to nearest intersection: ")
    (display
      (apply min (map (lambda (space)
                        (manhattan-distance-from-origin (car space)))
                      (set->list intersections))))
    (newline)
    (display "fewest combined steps to get to an intersection: ")
    (display
      (apply min (map (lambda (space)
                        (combined-distance-to-space space coverages))
                      (set->list intersections))))))
