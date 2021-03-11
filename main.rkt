#! /usr/bin/env racket

#lang typed/racket

(require/typed 2htdp/image
               [circle (-> Natural Symbol Any Any)]
               [rectangle (-> Natural Natural Symbol Any Any)]
               [beside (-> Any * Any)]
               [place-image (-> Any Real Real Any Any)])
                       
(require/typed rackunit
               [check-equal? (-> Any Any Void)])

;; Default colors and style
(define MODE : Symbol 'solid)
(define COLOR : String "cornflower blue")

(: dot (-> Natural Any))
(define (dot radius)
  (circle radius MODE COLOR))

;; test "dot"
(check-equal? (dot 10) (circle 10 MODE COLOR))
(check-equal? (dot 20)
              (circle 20 MODE COLOR))

;; Create a horizontal row of dots with
;; radius `radius`,
;; margin `margin`, and 
;; number `n`.
(: dots (-> Natural Integer Natural Any))
(define (dots radius n margin)
  (let ([u : Any (place-image
                  (dot radius)
                  (/ margin 2) (/ margin 2)
                  (rectangle (* margin 2) (* margin 2) 'solid "transparent"))])
    (if (= n 1)
        u
        (beside u (dots radius (- n 1) margin)))))
  
      
      



