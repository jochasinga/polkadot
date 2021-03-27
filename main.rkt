#! /usr/bin/env racket

#lang typed/racket

(define-type XPlace (U String Symbol))
(define-type YPlace (U String Symbol))

(require "./color.rkt")
(require/typed 2htdp/image
               [circle (-> Real Symbol Any Any)]
               [rectangle (-> Natural Natural Symbol Any Any)]
               [beside (-> Any * Any)]
               [above (-> Any * Any)]
               [overlay/align (-> XPlace YPlace Any * Any)]
               [x-place? (-> XPlace Boolean)]
               [y-place? (-> YPlace Boolean)]
               [image? (-> Any Boolean)]
               [place-image (-> Any Real Real Any Any)])
                       
(require/typed rackunit
               [check-equal? (-> Any Any Void)])

;; Default colors and style
(define MODE : Symbol 'solid)
(define COLOR : String "white")
(define DEFAULT_COLOR: (color "White" "f0f0f0"))

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
;  (let ([u : Any (place-image
;                  (dot radius)
;                  (/ margin 2) (/ margin 2)
;                  (rectangle (* margin 2) (* margin 2) 'solid "transparent"))])
  (let ([u : Any (overlay/align
                  'center 'middle
                  (dot radius)
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
           
             

  
      
      



