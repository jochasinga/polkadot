#lang typed/racket

(require/typed rackunit
               [check-equal? (-> Any Any Void)])

(provide shift)

(: shift (All (T) (->* ((Listof T) Integer) ((Listof T)) (Listof T))))
(define (shift ls n [acc '()])
  (append
   (take-right ls (- (length ls) n))
   (take ls n)))

(check-equal? (shift '(1 2 3 4 5) 2) '(3 4 5 1 2))