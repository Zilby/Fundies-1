;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |problem set 5|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;Homework 5
;;Alexander Zilbersher
;;Jialin Zhou

(require 2htdp/image)
(require 2htdp/universe)

;Problem 1

;#1
;An Item is one of:
;-a letter
;-a box

(define-struct letter (address weight))
;A letter is a (make-letter String Number)
;Interpretation: the weight of a letter must be less than 3.5 ounces
;the cost of a letter is 50 cents

(define-struct box (height width length weight))
;A box is a (make-box Number Number Number Number)
;Interpretation: the weight of a box must be less than
;or equal to 800 ounces
;The sum of the height, width, and length of a box
;must be less than or equal to 62 inches
;The product of the height, width, and length of a box
;must be less than or equal to 7938 cubic inches
;The cost of a box is 15 cents per ounce

;#2
;item-ok? takes a struct i that is an Item (either a struct letter or a struct box)
;and returns a boolean representing whether the item satisfies the conditions
;that are required by its respective struct type
;(define (item-ok?-tmpl i)
;  (cond
;    [(letter? i) ... (letter-address i) ... (letter-weight i) ...]
;    [(box? i) ... (box-height i) ... (box-width i) ... (box-length i) ... (box-weight i) ...]))

(define (item-ok? i)
  (cond
    [(letter? i) (cond
                   [(< (letter-weight i) 3.5) true]
                   [else false])]
    [(box? i) (cond
                [(and (<= (box-weight i) 800)
                      (<= (+ (box-height i) (box-width i) (box-length i)) 62)
                      (<= (* (box-height i) (box-width i) (box-length i)) 7938)) true]
                [else false])]
    [else false]))

(check-expect (item-ok? (make-letter "my house" 5)) false)
(check-expect (item-ok? (make-letter "your house" 2)) true)
(check-expect (item-ok? (make-box 10 15 25 600)) true)
(check-expect (item-ok? (make-box 10 15 25 900)) false)
(check-expect (item-ok? (make-box 25 15 25 600)) false)

;#3
;; LOI is one of:
; -(cons (make-letter String Number) LOI)
; -(cons (make-box Number Number Number Number) LOI)
; -'()
; An LOI represents a list of Items

;#4
;bad-items takes a list l that is a LOI 
;and returns a new LOI containing all the items
;that don't satisfy the conditions
;that are required by their respective struct type
;(define (bad-items-tmpl l)
;  (cond
;    [(empty? l) ... ]
;    [(item-ok? (first l)) ... (bad-items-tmpl (rest l))]
;    [else ... ]))

(define (bad-items l)
  (cond
    [(empty? l) l]
    [(item-ok? (first l)) (bad-items (rest l))]
    [else (cons (first l) (bad-items (rest l)))]))

(check-expect (bad-items (list
                          (make-letter "my house" 5)
                          (make-letter "your house" 2)
                          (make-box 10 15 25 600)
                          (make-box 10 15 25 900)
                          (make-box 25 15 25 600)))
              (cons (make-letter "my house" 5) (cons (make-box 10 15 25 900) (cons (make-box 25 15 25 600) '()))))

;#5
;total-postage takes a list l that is a LOI 
;and returns a Number representing the total cost
;of all the items contained within the give LOI
;using the given costs of their respective structs
;(define (total-postage-tmpl l)
;  (cond
;    [(empty? l) ...]
;    [(letter? (first l)) ... (letter-address (first l)) ... (letter-weight (first l)) ... (total-postage (rest l))]
;    [(box? (first l)) ... (box-height (first l)) ... (box-width (first l)) ... (box-length (first l)) ... (box-weight (first l)) ... (total-postage (rest l))]))

(define (total-postage l)
  (cond
    [(empty? l) 0]
    [(letter? (first l)) (+ 50 (total-postage (rest l)))]
    [(box? (first l)) (+ (* 15 (box-weight (first l))) (total-postage (rest l)))]))

(check-expect (total-postage (list
                          (make-letter "my house" 5)
                          (make-letter "your house" 2)
                          (make-box 10 15 25 10)
                          (make-box 10 15 25 100)
                          (make-box 25 15 25 400)))
              (+ 50 50 150 1500 6000))