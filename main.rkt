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
               [make-color (-> Number Number Number Any)]
;              [x-place? (-> XPlace Boolean)]
;              [y-place? (-> YPlace Boolean)]
;              [image? (-> Any Boolean)]
               [place-image (-> Any Real Real Any Any)])
                       
(require/typed rackunit
               [check-equal? (-> Any Any Void)])

;; Default colors and style
(define MODE : Symbol 'solid)

(: dot (->* (Real) (RGBList) Any))
(define (dot radius [c DEFAULT_COLOR])
  (circle radius
          MODE
          (make-color (car c)
                      (cadr c)
                      (caddr c))))

;; test "dot"
(check-equal? (dot 10) (circle 10 MODE "black"))
(check-equal? (dot 20)
              (circle 20 MODE "black"))
(check-equal? (dot 20 (list 234 45 120))
              (circle 20 MODE (make-color 234 45 120)))
(check-equal? (dot 20 (hex->rgb "ffffff"))
              (circle 20 MODE (make-color 255 255 255)))


(: dots-aux (-> Real Integer Natural (Listof RGBList) (Listof RGBList) Any))
(define (dots-aux radius n margin colors acc)
  (let ([u : Any (overlay/align
                  'center 'middle
                  (dot radius (car acc))
                  (rectangle (* margin 2)
                             (* margin 2)
                             'solid
                             "transparent"))])
    (if (= n 1)
        u
        (beside u (dots-aux radius
                            (- n 1)
                            margin
                            colors
                            (if (null? (cdr acc))
                                colors
                                (cdr acc)))))))


;; Create a horizontal row of dots with
;; radius `radius`,
;; margin `margin`, and 
;; number `n`.
(: dots (->* (Real Integer Natural) ((Listof RGBList)) Any))
(define (dots radius n margin [colors (list DEFAULT_COLOR)])
  (dots-aux radius n margin colors colors))


(: shift (All (T) (->* ((Listof T) Integer) ((Listof T)) (Listof T))))
(define (shift ls n [acc '()])
  (append
   (take-right ls (- (length ls) n))
   (take ls n)))

(check-equal? (shift '(1 2 3 4 5) 2) '(3 4 5 1 2))
  
;; Create a matrix of alternating rows of dots
(: mdots (->* (Real Integer Natural Natural) ((Listof RGBList)) Any))
(define (mdots radius nx ny margin [colors (list DEFAULT_COLOR)])
  (let ([a (dots radius nx margin colors)]
        [b (dots radius (- nx 1) margin (shift colors (- (length colors) 1)))])
    (if (= ny 0)
        a
        (above
         (if (= 0 (modulo ny 2))
             a
             b)
         (mdots radius nx (- ny 1) margin colors)))))
           
             

  
      
      



