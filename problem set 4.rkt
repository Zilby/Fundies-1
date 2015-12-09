;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |problem set 4|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;Homework 4
;;Alexander Zilbersher
;;Ashley Moran

(require 2htdp/image)
(require 2htdp/universe)

;Problem 1

(define-struct regular (id price years))
; A regular is a structure:
;    (make-regular String Number Number)
; interpretation: (make-regular id price years) represents
; a regular movie indicating its id, price and the number of
; years since its been in stock
(define-struct classic (id price))
; A classic is a structure:
;    (make-regular String Number)
; interpretation: (make-classic id price) represents
; a classic movie indicating its id, and price. 

; get-price takes a struct movie (which can be either a regular
; or a classic) and returns the price of that movie, either based on
; the number of years and original price in the case of a regular
; or just the number of years in the case of a classic
;(define (get-price-templ movie)
;  (cond
;    [(regular? movie)
;     (regular-price movie) ... (regular-years movie) ... (regular-id movie)]
;    [(classic? movie) ... (classic-price movie) ... (classic-id movie)]
;    [else ... ]))

(define (get-price movie)
  (cond
    [(regular? movie)
     (cond
       [(< (* (regular-price movie) (- 1 (* 0.035 (regular-years movie)))) 2) 2]
       [else (* (regular-price movie) (- 1 (* 0.035 (regular-years movie))))])]
    [(classic? movie) (classic-price movie)]
    [else "This is not a movie"]))

(check-expect (get-price (make-regular "827-46-4FZ" 10 3)) (- 10(* 10 0.105)))
(check-expect (get-price (make-classic "562-37-9TQ" 15)) 15)

;Problem 2

;; Shape is one of:
;; -- Circle
;; -- Square
;; -- Rectangle
 
(define-struct circl [x y r outline c])
;; A Circle is a (make-circl Number Number Number Boolean Symbol)
;; interpretation: x and y determine the center of the circle,
;;   r the radius, outline whether it's outlined or solid,
;;   and c its color
 
(define-struct squar [x y size outline c])
;; A Square is a (make-squar Number Number Number Boolean Symbol)
;; interpretation: x and y determine the center of the square,
;;   size the length of the square's sides, outline whether it's outlined or solid,
;;   and c its color
 
(define-struct recta [x y width height outline c])
;; A Rectangle is a (make-recta Number Number Number Number Boolean Symbol)
;; interpretation: x and y determine the center of the rectangle,
;;   width the length of the rectangle's bottom and top sides,
;;   height the length of the rectangle's right and left sides
;;   outline whether it's outlined or solid,
;;   and c its color

; shape-shift takes a struct s that is a shape and a Number d
; and returns a new struct of the same type with d added to its
; x value
;(define (shape-shift-x-templ s d)
;  (cond
;    [(circl? s) ... (circl-x s) ... d ... (circl-y s) ... (circl-r s) ... (circl-outline s) ... (circl-c s) ...]
;    [(squar? s) ... (squar-x s) ... d ... (squar-y s) ... (squar-size s) ... (squar-outline s) ... (squar-c s) ...]
;    [(recta? s) ... (recta-x s) ... d ... (recta-y s) ... (recta-width s) ... (recta-height s) ... (recta-outline s) ... (recta-c s)...]))

(define (shape-shift-x s d)
  (cond
    [(circl? s) (make-circl (+ (circl-x s) d) (circl-y s) (circl-r s) (circl-outline s) (circl-c s))]
    [(squar? s) (make-squar (+ (squar-x s) d) (squar-y s) (squar-size s) (squar-outline s) (squar-c s))]
    [(recta? s) (make-recta (+ (recta-x s) d) (recta-y s) (recta-width s) (recta-height s) (recta-outline s) (recta-c s))]))

; shape-in? takes a struct s that is a shape and a Posn p
; and returns a boolean indicating whether p is within the
; boundaries of s
;(define (shape-in?-templ s p)
;  (cond
;    [(circl? s) ... p ... (circl-x s) ... (circl-y s) ... (circl-r s) ... (circl-outline s) ... (circl-c s) ...]
;    [(squar? s) ... p ... (squar-x s) ... (squar-y s) ... (squar-size s) ... (squar-outline s) ... (squar-c s) ...]
;    [(recta? s) ... p ... (recta-x s) ... (recta-y s) ... (recta-width s) ... (recta-height s) ... (recta-outline s) ... (recta-c s)...]))

(define (shape-in? s p)
  (cond
    [(circl? s)
     (cond
       [(> (circl-r s) (sqrt(+ (sqr (- (posn-x p) (circl-x s))) (sqr (- (posn-y p) (circl-y s)))))) true]
       [else false])]
    [(squar? s)
     (cond
       [(and (and(> (posn-x p) (- (squar-x s) (/ (squar-size s) 2)))(< (posn-x p) (+ (squar-x s) (/ (squar-size s) 2))))
             (and(> (posn-y p) (- (squar-y s) (/ (squar-size s) 2)))(< (posn-y p) (+ (squar-y s) (/ (squar-size s) 2))))) true]
       [else false])]
    [(recta? s)
     (cond
       [(and (and(> (posn-x p) (- (recta-x s) (/ (recta-width s) 2)))(< (posn-x p) (+ (recta-x s) (/ (recta-width s) 2))))
             (and(> (posn-y p) (- (recta-y s) (/ (recta-height s) 2)))(< (posn-y p) (+ (recta-y s) (/ (recta-height s) 2))))) true]
       [else false])]))

; shape-draw takes a struct s that is a shape and a scene sc
; and returns an image of s placed on the scene given the coordinates
; contained within its respective struct
;(define (shape-draw-templ s sc)
;  (cond
;    [(circl? s) ... sc ... (circl-x s) ... (circl-y s) ... (circl-r s) ... (circl-outline s) ... (circl-c s) ...]
;    [(squar? s) ... sc ... (squar-x s) ... (squar-y s) ... (squar-size s) ... (squar-outline s) ... (squar-c s) ...]
;    [(recta? s) ... sc ... (recta-x s) ... (recta-y s) ... (recta-width s) ... (recta-height s) ... (recta-outline s) ... (recta-c s)...]))

(define (shape-draw s sc)
  (cond
    [(circl? s) (place-image (circle (circl-r s) (cond [(circl-outline s) "outline"] [else "solid"]) (circl-c s)) (circl-x s) (circl-y s) sc)]
    [(squar? s) (place-image (square (squar-size s) (cond [(squar-outline s) "outline"] [else "solid"]) (squar-c s)) (squar-x s) (squar-y s) sc)]
    [(recta? s) (place-image (rectangle (recta-width s) (recta-height s) (cond [(recta-outline s) "outline"] [else "solid"]) (recta-c s)) (recta-x s) (recta-y s) sc)]))
  

;; ... problem solving steps ...
 
;; inspect for expected results:
(define sh (make-squar 100 100 50 true 'red))
(define pt (make-posn  130 130))

(check-expect (circl-x (shape-shift-x (make-circl 20 30 40 true "red") 10)) 30)

;squar side lengths should go from 75-125 on both the x and y planes
;thus 130 130 should not be within the square
(check-expect (shape-in? sh pt) false)

(shape-draw (make-circl 130 130 5 true 'red)
            (shape-draw sh
                        (empty-scene 300 300)))

;Problem 3

; passwords-6-11? takes a list of Strings p
; and returns a boolean indicating whether the Strings contained
; within P are at least 6 characters long, but less than 11 characters
;(define (passwords-6-11?-templ p)
;  (cond
;    [...p... (first p) ... (rest p) ... (passwords-6-11?-templ (rest p))])]
;    [else ... ]))

(define (passwords-6-11? p)
  (cond
    [(and (> (string-length (first p)) 5) (< (string-length (first p)) 11))
     (cond [(null? (rest p)) true]
           [else (passwords-6-11? (rest p))])]
    [else false]))

(check-expect (passwords-6-11? (list "sldkgjsl" "slkgsdgooi" "adsfas" "asdfasdf")) true)
(check-expect (passwords-6-11? (list "sldkgjsl" "slkgsdgooi" "adsfas" "asdsdlfkjsdkfasdf")) false)
(check-expect (passwords-6-11? (list "sldkgjsl" "slkgsdgooi" "aas" "asdfasdf")) false) 

; passwords-ok? takes a list of Strings p and two Numbers, min and max
; and returns a boolean indicating whether the Strings contained
; within P are at least min characters long, but less than max characters
;(define (passwords-ok?-templ p)
;  (cond
;    [...p... min ... max ... (first p) ... (rest p) ... (passwords-ok?-templ (rest p))])]
;    [else ... ]))

(define (passwords-ok? p min max)
  (cond
    [(and (> (string-length (first p)) (- min 1)) (< (string-length (first p)) max))
     (cond [(null? (rest p)) true] [else (passwords-ok? (rest p) min max)])]
    [else false]))

(check-expect (passwords-ok? (list "sldk" "slk" "adsf" "asd") 3 5) true)

;Problem 4

(define-struct ball [x y color])
; A ball is a structure:
;    (make-ball Number Number String)
; interpretation: (make-ball x y color) represents
; a small circle of color 'color' displayed at the coordinates
; x and y

;; Ball = (make-ball Number Number Color)
;; Color is one of 'red, 'yellow, 'blue, etc.
;; LoB is one of:
;; -(cons (make-ball Number Number Color) LoB)
;; -'()
;; An LoB represents a list of balls

; lob-length takes a LoB lob returns a number indicating how many balls
; are contained with lob. 
;(define (lob-length-templ lob)
;  (cond
;    [(null? lob) ... ]
;    [else ... (lob-length-templ (rest lob))]))

(define (lob-length lob)
  (cond
    [(null? lob) 0]
    [else (+ 1 (lob-length (rest lob)))]))

(check-expect (lob-length (list (make-ball 4 7 "red") (make-ball 4 7 "red") (make-ball 4 7 "red") (make-ball 4 7 "red"))) 4)

; lob-x takes a LoB lob returns a list of all the
; x values contained within the balls within lob
;(define (lob-x-templ lob)
;  (cond
;    [(null? lob) ... ]
;    [else ... (ball-x (first lob)) ... (ball-y (first lob)) ...
;     (ball-color (first lob)) ... (lob-x-templ (rest lob))]))

(define (lob-x lob)
  (cond
    [(null? lob) lob]
    [else (cons (ball-x (first lob)) (lob-x (rest lob)))]))

(check-expect (lob-x (list (make-ball 4 7 "red") (make-ball 5 7 "red") (make-ball 6 7 "red") (make-ball 7 7 "red")))
(cons 4 (cons 5 (cons 6 (cons 7 '())))))

; lob-draw takes a LoB lob and produces circle images
; based on the values within the balls within lob and
; places them on an empty scene at their given coordinates
;(define (lob-draw-templ lob)
;  (cond
;    [(null? lob) ...]
;    [else ... (ball-color (first lob))
;          ... (ball-x (first lob))
;          ... (ball-y (first lob))
;          ... (lob-draw-templ (rest lob))]))

(define (lob-draw lob)
  (cond
    [(null? lob) (empty-scene 300 300)]
    [else (place-image
           (circle 3 "solid" (ball-color (first lob)))
           (ball-x (first lob)) (ball-y (first lob))
           (lob-draw (rest lob)))]))

; lob-filter takes a LoB lob and produces a new
; lob only containing only the balls from the
; original lob whose coordinates are between 0 and 300
;(define (lob-filter-templ lob)
;  (cond
;    [(null? lob) ... ]
;    [... (ball-x (first lob)) ... (ball-y (first lob)) ...  (ball-color (first lob)) ...
;     (lob-filter-templ (rest lob))]

(define (lob-filter lob)
  (cond
    [(null? lob) lob]
    [(or (> (ball-x (first lob)) 300) (< (ball-x (first lob)) 0)
         (> (ball-y (first lob)) 300) (< (ball-y (first lob)) 0))
     (lob-filter (rest lob))]
    [else (cons (first lob) (lob-filter (rest lob)))]))

(check-expect (lob-filter (list (make-ball 40 70 "blue") (make-ball 150 100 "red") (make-ball 260 230 "orange") (make-ball 170 170 "green") (make-ball 600 530 "purple")))
              (cons (make-ball 40 70 "blue") (cons (make-ball 150 100 "red") (cons (make-ball 260 230 "orange") (cons (make-ball 170 170 "green") '())))))

(lob-draw (lob-filter (list (make-ball 40 70 "blue") (make-ball 150 100 "red") (make-ball 260 230 "orange") (make-ball 170 170 "green") (make-ball 600 530 "purple"))))

; lob-member takes a LoB lob and a ball b and returns
; a boolean indicating whether lob contains b as one of
; its values
;(define (lob-member?-templ lob b)
;  (cond
;    [(null? lob) ... ]
;    [(equal? ((first lob) b)) ... (ball-x b) ... (ball-y b) ... (ball-color b)
;     ... (ball-x (first lob)) ... (ball-y (first lob) ... (ball-color (first lob))
;    ... (lob-member?-templ (rest lob) b)]))

(define (lob-member? lob b)
  (cond
    [(null? lob) false]
    [(equal? (first lob) b) true]
    [else (lob-member? (rest lob) b)]))

(check-expect (lob-member? (lob-filter (list (make-ball 40 70 "blue") (make-ball 150 100 "red")
                                             (make-ball 260 230 "orange") (make-ball 170 170 "green")
                                             (make-ball 600 530 "purple"))) (make-ball 40 70 "blue")) true)
(check-expect (lob-member? (lob-filter (list (make-ball 40 70 "blue") (make-ball 150 100 "red")
                                             (make-ball 260 230 "orange") (make-ball 170 170 "green")
                                             (make-ball 600 530 "purple"))) (make-ball 600 530 "purple")) false)

;Problem 5

(define-struct txt [content x y])
;; Txt = (make-txt String Number Number)
;; Represents the occurrence of the given text at the given location,
;; in computer-graphics coordinates.
 
;; LoTxt is one of:
;; -- empty
;; -- (cons Txt LoTxt)
 
(define-struct world [image hidden])
;; World = (make-world Image LoTxt)
;; intepretation:
;;  The world's image represents the image that the audience can see.
;;  The world's list of Txt represents the yet-to-be-revealed elements.

(define w (make-world (empty-scene 400 400) (cons (make-txt "On your mark." 140 150) (cons (make-txt "Get set." 190 190) (cons (make-txt "Go!" 240 230) '())))))

; display takes a struct world w and returns its image
; (define (display-templ w)
;   (... w))

(define (display w)
  (world-image w))

; next takes a world w and returns a new world
; with the txts within the LoTxt within the world
; converted to a text images and placed on w's image 
;(define (lob-member?-templ lob b)
;  (cond
;    [(null? lob) ... ]
;    [(equal? ((first lob) b)) ... (ball-x b) ... (ball-y b) ... (ball-color b)
;     ... (ball-x (first lob)) ... (ball-y (first lob) ... (ball-color (first lob))
;    ... (lob-member?-templ (rest lob) b)]))

(define (next w)
  (cond
    [(null? (world-hidden w)) w]
  [else
   (make-world
    (place-image
     (text (txt-content (first (world-hidden w))) 20 "blue")
     (txt-x (first (world-hidden w)))(txt-y (first (world-hidden w)))
     (world-image w))
    (rest (world-hidden w)))]))

(big-bang w
          (on-tick next 1)
          (to-draw display))