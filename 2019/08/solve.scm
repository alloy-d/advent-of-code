(import (chicken io)
        (chicken port)
        (chicken string)
        srfi-1)

(define input-file "08/input")

(define (digitchar->integer char)
  (- (char->integer char) 48))

(define (layer-parser width height)
  (lambda (digit-string)
    (chop (map digitchar->integer (string->list (string-chomp digit-string)))
          width)))

(define (read-image width height)
  (port-map (layer-parser width height)
            (cut read-string (* width height))))

(with-input-from-string "123456789012" (cut read-image 3 2))

(define image
  (butlast
    (with-input-from-file input-file (cut read-image 25 6))))

; takes many shortcuts because I know the digits are 0, 1, or 2.
(define (count-digits-in-row row)
  (foldl (lambda (counts digit)
           (vector-set! counts digit (+ 1 (vector-ref counts digit)))
           counts)
         (make-vector 3 0)
         row))
(count-digits-in-row (caar image))

; returns a list not because it makes sense, but because it's
; convenient.
(define (vector-sum vectors)
  (map (cut foldl + 0 <>)
       (apply zip (map vector->list vectors))))

(define (count-digits-in-layer layer)
  (vector-sum (map count-digits-in-row layer)))
(count-digits-in-layer (car image))

(let* ((counts-by-layer (map count-digits-in-layer image))
       (min-zeroes (apply min (map car counts-by-layer)))
       (counts-for-layer-with-min-zeroes (assq min-zeroes counts-by-layer)))
  (list 'min-zeroes min-zeroes
        'counts-for-layer counts-for-layer-with-min-zeroes
        'part-a-solution (apply * (drop counts-for-layer-with-min-zeroes 1))))
