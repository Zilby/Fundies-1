;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname problemset2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Ashley Moran
;; Alexander Zilbersher
(require 2htdp/image)

;;Problem 1

(define (favorite-star side-length color sq-length)
  (overlay (star sq-length "solid" color) (square side-length "solid" "black")))

;;Problem 2
;; HtDP/2e excercise 44
; Worldstate is a Number
; interpretation the number of pixels between left border and the rightmost edge of the car


;;Problem 3

(define (humid? percent)
  (cond [(< percent 20) "dry"]
        [(> percent 65) "humid"]
        [else "comfortable"]))

;;Problem 4

(define BG (empty-scene 200 200))
(define bluedot (circle 4 "solid" "blue"))
(define greedot (circle 4 "solid" "green"))
(define redot (circle 4 "solid" "red"))

(define (translator red blue)
  (add-line (place-image
             bluedot
             (posn-x blue) (posn-y blue)
             (place-image
              greedot
              (+ (* .9 (- (posn-x blue) (posn-x red))) (posn-x red))
              (+ (* .9 (- (posn-y blue) (posn-y red))) (posn-y red))
              (place-image
              redot
              (posn-x red) (posn-y red) BG)))
            (posn-x blue) (posn-y blue)
            (posn-x red) (posn-y red)
            "black"))



(translator (make-posn 150 150) (make-posn 50 50))