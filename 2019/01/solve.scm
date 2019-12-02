(import (chicken io))

(define (fuel-requirement mass)
  (floor (- (/ mass 3) 2)))

(define (main args)
  (let ((nums (map string->number (read-lines))))
    (display (foldl + 0 (map fuel-requirement nums)))))
