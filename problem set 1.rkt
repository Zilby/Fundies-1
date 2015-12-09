;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |problem set 1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;Alexander Zilbersher
;Nat Tuck 9/17/15 4:35pm-5:40pm
"Problem 1" 

(define cost (+ 44620 910 14472))
(define lecture (/ cost (* 4 2 3 13)))
cost
lecture

"Problem 2"

(define (Insulin bsv)
  (cond
    [(< (- bsv 110) 0) 0]
    [else (quotient (- bsv 95) 15)]))

(string-append "140 BSV = "(number->string(Insulin 140))" units")
(string-append "180 BSV = "(number->string(Insulin 180))" units")

"Here's a table:"

(define (make-table n l)
  (cond
    [(> n 200) l]
    [else (make-table (+ n 10) (append l (list (string-append (number->string n) " | " (number->string(Insulin n))))))]))

(define table (make-table 100 (list "BSV | Insulin" "-----------")))
(car table)
(cadr table)
(caddr table)
(cadddr table)
(cadr(cdddr table))
(caddr(cdddr table))
(cadddr(cdddr table))
(cadr (cdddr (cdddr table)))
(caddr (cdddr (cdddr table)))
(cadddr (cdddr (cdddr table)))
(cadr (cdddr(cdddr (cdddr table))))
(caddr (cdddr(cdddr (cdddr table))))
(cadddr (cdddr(cdddr (cdddr table))))
;(I had too much fun with cadr)
;(This is what happens when I can't
;use recursion because beginning student)
(string-append "290 BSV = "(number->string(Insulin 290))" units")

"Problem 3"

(define (accelerate x)
  (+(* 20.75 x x) 13))

(accelerate 0)
(accelerate 1)
(accelerate 2)
(accelerate 3)
(accelerate 4)
(accelerate 5)
(string-append "After 6 seconds the ball goes "
(number->string(accelerate 6)) " cm")

"Problem 4"

;w being weeks, c being containers
;handled in the given time period
(define (pay w c)
  (+ (* 10 w) (* 4 c)))
;eg: 14 weeks (~3 months) and 28 containers
;(~2 containers per week)
(string-append "For 14 weeks and 2 containers per week the girl gets "
(number->string(pay 14 28)) " dollars")

"Problem 5"

(define (greeting s)
  (string-append "Dear " s ":"))

(greeting "Prof. Tuck")

"Problem 6"
(require 2htdp/image)
(define (my-star c)
(star 12 "solid" c))
(my-star "blue")