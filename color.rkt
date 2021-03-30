#lang typed/racket

(require/typed rackunit
               [check-equal? (-> Any Any Void)])

(provide hex->rgb
         color->rgb
;        color
;        struct:color
;        make-color
;        color-name
;        color-hex
         RGB
         RGBList
         new-RGB
         DEFAULT_COLOR
         ROSETTE_AND_CREAM
         ROSY_FLAMINGO
         RETRO_PUNCH
         THE_DEEP_BLUE
         THWARTED_SUMMER_SHOWER
         BIRTH_SPRING
         SAN_FRANCISCO_CLOUDED)

;; An assoc list like '(('r 255) ('g 255) ('b 0))
(define-type RGB (Listof (Pairof Symbol Number)))
(: new-RGB (->* (Byte Byte Byte) RGB))
(define (new-RGB r g b)
  (list (cons 'r r) (cons 'g g) (cons 'b b)))

(struct color
  ([name : (U String Symbol)]
   [hex : String])
  #:transparent
  #:extra-constructor-name make-color)

(define ROSETTE_AND_CREAM
  : (Listof color)
  (list (color "Rosewater" "D8A7B1")
        (color "Spearmint" "B6E2D3")
        (color "Cream" "FAE8E0")
        (color "Hot Pink" "EF7C8E")))

(define ROSY_FLAMINGO
  : (Listof color)
  (list (color "Coffee Pot" "67595E")
        (color "Coffee Pot" "A49393")
        (color "Dusty Rose" "EED6D3")
        (color "Rosewater" "E8B4B8")))

(define RETRO_PUNCH
  : (Listof color)
  (list (color "Chili Pepper" "F51720")
        (color "Pink" "FA26A0")
        (color "Gold" "F8D210")
        (color "Cyan" "2FF3E0")))

(define THE_DEEP_BLUE
  : (Listof color)
  (list (color "Baby Blue" "7EC8E3")
        (color "Blue" "0000FF")
        (color "Navy Blue" "000C66")
        (color "Dark Blue" "050A30")))

(define THWARTED_SUMMER_SHOWER
  : (Listof color)
  (list (color "Cornflower" "4C5270")
        (color "Hot Pink" "F652A0")
        (color "Cyan" "36EEE0")
        (color "Tiffany Blue" "BCECE0")))

(define BIRTH_SPRING
  : (Listof color)
  (list (color "Rosewater" "E4B4B4")
        (color "Sage Green" "9DA993")
        (color "Misty Blue" "E3E8E9")
        (color "Nude" "BCA88E")))

(define SAN_FRANCISCO_CLOUDED
  : (Listof color)
  (list (color "Misty Blue" "2F5061")
        (color "Coral" "E57F84")
        (color "Ivory" "F4EAE6")
        (color "Teal Green" "4297A0")))

(: color->rgb (-> (Listof color) (Listof RGBList)))
(define (color->rgb color-list)
  (map
   (λ (s) (hex->rgb s))
   (map
    (λ (c) (color-hex c))
    color-list)))


(define DEFAULT_COLOR : RGBList '(0 0 0))
(define MIN_HEX_DIGIT : Integer 2)
(define MAX_HEX_DIGIT : Integer 6)

;; Auxiliary function that takes care
;; of the nitty-gritty of parsing the string.
;; (define-type RGBList (Listof (U Char Complex False)))
(define-type RGBList (Listof Number))
(: aux (-> (Listof Char) RGBList RGBList))
(define (aux cs acc)
  (if (null? cs)
      acc
      (let ([acc_
             (filter
               (λ (x) (complex? x))
               (cons (string->number
                      (list->string
                       (cons #\#
                             (cons #\x
                                   (list (car cs)
                                         (car (cdr cs)))))))
                     acc))])
        (aux (cdr (cdr cs)) acc_))))

;; Converts a color hex value to a list
;; of RGB integer list.
(: hex->rgb (-> String RGBList))
(define (hex->rgb hex)
  (if (or (< (string-length hex) MIN_HEX_DIGIT)
          (> (string-length hex) MAX_HEX_DIGIT))
      (raise-argument-error
       'hex->rgb
       "string with length between 2 to 6"
       hex)
      (let ([cs (string->list hex)])
        (reverse (aux (string->list hex) '())))))


;; Converts color struct to RGB assoc list.
(: color->RGB (-> color RGB))
(define (color->RGB c)
  (let ([rgb (hex->rgb (color-hex c))])
    (if (< (length rgb) 3)
        (raise-argument-error
         'color-hex
         "rgb list needs to have length of 3"
         rgb)
        (list (cons 'r (car rgb))
              (cons 'g (car (cdr rgb)))
              (cons 'b (car (cdr (cdr rgb))))))))

;;;;;;;;;;;;;;;;;;; Tests ;;;;;;;;;;;;;;;;;;;;;;;;


(check-equal? (color->RGB (color "white" "ffffff"))
              (list (cons 'r 255) (cons 'g 255) (cons 'b 255)))
(check-equal? (color->RGB (color "black" "000000"))
              (list (cons 'r 0) (cons 'g 0) (cons 'b 0)))

(check-equal? (hex->rgb "FF") '(255))
(check-equal? (hex->rgb "ff") '(255))
(check-equal? (hex->rgb "f0") '(240))
(check-equal? (hex->rgb "f7f7F0") '(247 247 240))

(check-equal? (with-handlers ([exn:fail:contract?
                   (λ (e) "error")])
                (hex->rgb ""))
              "error")
(check-equal? (with-handlers ([exn:fail:contract?
                   (λ (e) "error")])
                (hex->rgb "f0f0f0f"))
              "error")