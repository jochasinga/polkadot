#! /usr/bin/env racket

#lang typed/racket

(require/typed 2htdp/image
               [circle (-> Natural Symbol Any Any)])
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

(: dots (-> Natural Natural Any))
(define (dots n dist)
  (let aux (lambda 



