;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname lab10) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; Constants
;(define WIDTH 300)
(define WIDTH 500)
(define HEIGHT 160)
(define MT (empty-scene WIDTH HEIGHT))

; next-size : Number -> Number
;(define (next-size s)
;  (/ s 2))

(define (next-size s)
  (* 2(/ s 3)))


(define (circles x size scn)
  (cond
    [(< size 2) scn]
    [else (place-image
           (circle size "outline" "blue")
           x 80
           (circles (+ x (+ size (next-size size))) (next-size size) scn))]))

;(circles 100 60 MT)

(define (next-x x ang size)
  (+ x (*(+ size (next-size size)) (cos ang))))

(define (next-y y ang size)
  (+ y (* (+ size (next-size size)) (sin ang))))

;(define (next-ang ang)
;  (+ ang (/ pi 10)))

;for modified spiral
(define (next-ang ang)
  (+ ang (/ pi 4)))

#|
(define (spiral x size y ang scn)
  (cond
    [(< size 2) scn]
    [else (place-image
           (circle size "outline" "blue")
           x y
           (spiral (next-x x ang size)
                   (next-size size)
                   (next-y y ang size)
                   (next-ang ang) scn))]))
|#

;modified spiral
(define (spiral x size y ang scn)
  (cond
    [(> ang 100) scn]
    [else (place-image
           (circle size "outline" "blue")
           x y
           (spiral (next-x x ang size)
                   size
                   (next-y y ang size)
                   (next-ang ang) scn))]))

;(spiral 100 60 80 -0.3 MT)
;(spiral 80 30 50 0 (empty-scene 200 200))

;_______________________________________________________


; put-line : Number Number Number Number String Scene -> Scene
; Put a line in the scene starting at (x,y) len distance in the given direction
;   with the given color
(define (put-line x y ang len color scn)
  (place-image (line (* (cos ang) len)
                     (* (sin ang) len) color)
               (+ x (* (cos ang) (/ len 2)))
               (+ y (* (sin ang) (/ len 2))) scn))

(define (tree x y ang len scn)
  (cond
    [(< len 3) (put-line x y ang len "green" scn)]
    [else
     (put-line x y ang len "brown"
               (tree (+ x (* (/ len 3) (cos ang)))
                     (+ y (* (/ len 3) (sin ang)))
                     (+ ang (/ pi 6))
                     (/ len 2)
                     (tree
                      (+ x (* 2 (/ len 3) (cos ang)))
                      (+ y (* 2 (/ len 3) (sin ang)))
                      (- ang (/ pi 6))
                      (/ len 2)
                      scn)))]))

;(tree 30 150 0 150 (empty-scene 300 300))

(define (koch x y ang len iter scn)
  (cond
    [(= 0 iter) (put-line x y ang len "blue" scn)]
    [else
     (koch x
           y
           ang (/ len 3) (- iter 1)
           (koch (+ x (* 2 (/ len 3) (cos ang)))
                 (+ y (* 2 (/ len 3) (sin ang)))
                 ang (/ len 3) (- iter 1)
                 (koch (+ x (* (/ len 3) (cos ang)))
                       (+ y (* (/ len 3) (sin ang)))
                       (- ang (/ pi 3))
                       (/ len 3) (- iter 1)
                       (koch (+ x (* 2 (/ len 3) (cos ang)))
                             (+ y (* 2 (/ len 3) (sin ang)))
                             (- ang (* 2 (/ pi 3)))
                             (/ len 3) (- iter 1) scn))))]))

;(koch 10 180 0 580 0 (empty-scene 600 200))
;(koch 10 180 0 580 1 (empty-scene 600 200))
;(koch 10 180 0 580 2 (empty-scene 600 200))
;(koch 10 180 0 580 3 (empty-scene 600 200))
(koch 10 180 0 580 4 (empty-scene 600 200))