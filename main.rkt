#! /usr/bin/env racket

#lang typed/racket

(require/typed 2htdp/image
               [circle (-> Real Symbol Any Any)]
               [rectangle (-> Natural Natural Symbol Any Any)]
               [beside (-> Any * Any)]
               [above (-> Any * Any)]
               [place-image (-> Any Real Real Any Any)])
                       
(require/typed rackunit
               [check-equal? (-> Any Any Void)])

;; Default colors and style
(define MODE : Symbol 'solid)
(define COLOR : String "cornflower blue")

(: dot (-> Real Any))
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
(: dots (-> Real Integer Natural Any))
(define (dots radius n margin)
  (let ([u : Any (place-image
                  (dot radius)
                  (/ margin 2) (/ margin 2)
                  (rectangle (* margin 2) (* margin 2) 'solid "transparent"))])
    (if (= n 1)
        u
        (beside u (dots radius (- n 1) margin)))))

;; Create a matrix of alternating rows of dots
(: mdots (-> Real Integer Natural Natural Any))
(define (mdots radius nx ny margin)
  (let ([a (dots radius nx margin)]
        [b (dots radius (- nx 1) margin)])
    (if (= ny 0)
        a
        (above
         (if (= 0 (modulo ny 2))
             a
             b)
         (mdots radius nx (- ny 1) margin)))))
           
             

  
      
      



