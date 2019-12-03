(import (chicken io))

(define (fuel-requirement mass)
  (floor (- (/ mass 3) 2)))

(define (fuel-requirement-including-fuel mass)
  (let ((fuel (fuel-requirement mass)))
    (if (>= 0 fuel)
        0
        (+ fuel (fuel-requirement-including-fuel fuel)))))

(define (main args)
  (let ((masses (map string->number (read-lines))))
    (let ((fuel-for-masses (foldl + 0 (map fuel-requirement-including-fuel masses))))
      (display fuel-for-masses))))
