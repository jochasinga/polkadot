#lang typed/racket

(require/typed rackunit
               [check-equal? (-> Any Any Void)])

(provide hex->rgb
         color
         ROSETTE_AND_CREAM)

(struct color
  ([name : (U String Symbol)]
   [hex : String])
  #:transparent
  #:extra-constructor-name make-color)

(define ROSETTE_AND_CREAM
  : (Listof color)
  (list (color "Rosewater" "D8AB1")
        (color "Spearmint" "B6E2D3")
        (color "Cream" "FAE8E0")
        (color "Hot Pink" "EF7C8E")))

(define MIN_HEX_DIGIT : Integer 2)
(define MAX_HEX_DIGIT : Integer 6)

;; Auxiliary function that takes care
;; of the nitty-gritty of parsing the string.
(define-type RGBList (Listof (U Char Complex False)))
(: aux (-> (Listof Char) RGBList RGBList))
(define (aux cs acc)
  (if (null? cs)
      acc
      (let ([acc_ (cons (string->number
                         (list->string
                          (cons #\#
                                (cons #\x
                                      (list (car cs)
                                            (car (cdr cs)))))))
                        acc)])
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


;;;;;;;;;;;;;;;;;;; Tests ;;;;;;;;;;;;;;;;;;;;;;;;


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