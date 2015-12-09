;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname homework3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;Homework 3
;;Alexander Zilbersher
;;Ashley Moran
(require 2htdp/image)
(require 2htdp/universe)

;;Problem 1
#|design recipe for functions
1 data definition
2 signature (what type of data goes in and what type of data comes out) and purpose statement(comment which states the purpose of said function)
examples/ tests(check-expect) 
3 template-take inventory of what’s available to design the function-- takes the shape of the data---only concerned with input not output
4 code
5 test|#


;;Problem 1
;;lecture-hall
;;+------------------+---------------------+-----------------------+---------------+
;;|make-lecture-hall | lecture-hall-number | lecture-hall-capacity | lecture-hall? | 
;;+------------------+---------------------+-----------------------+---------------+
;; A lecture-hall is (make-lecture-hall (number number))
;; a number is a section number
;; a capacity is the amount of students the section can hold

;; at-capacity compares whether a lecture hall's capacity is greater than the current
;; number of expected students n
;; takes (struct number) returns boolean
;;(define (at-capacity-tmpl l n)
;;  (cond
;;    [(... l n) ...]
;;    [else ...]))


;;automobile
;;+----------------+-----------------+-----------------+------------------+-------------+
;;|make-automobile | automobile-year | automobile-make | automobile-model | automobile? |
;;+----------------+-----------------+-----------------+------------------+-------------+
;;An automobile is a (make-automobile (number string string))
; a year is the year it was made
; a make is the style of the car
; a model is the type of car

;; upgrade takes an automobile and changes its model to the most current one depending on what
;; its current model is returning the new automobile
;; takes (struct string) returns struct
;;(define (upgrade-tmpl auto mod)
;;  (cond
;;    [(= ... auto ... string) ...]
;;    [...][...][...]))


;;football-player
;;+---------------------+----------------------+--------------------------+------------------------+------------------+
;;|make-football-player | football-player-name | football-player-position | football-player-number | football-player? |
;;+---------------------+----------------------+--------------------------+------------------------+------------------+
;;A football-player is a (make-football-player (string string number))
;; a name is the name of the football player
;; a position is the position the football player plays on the team
;; a number is the number on the football players jersey

;; reassign takes a football-player and changes its position and number to the given
;; and returning the new football-player
;; takes (struct string number) returns struct
;;(define (reassign-tmpl footballp n p)
;;  (make-football-player (n p (... footballp)...)))


;;shirt
;;+-----------+----------------+------------+-------------+--------+
;;|make-shirt | shirt-material | shirt-size | shirt-color | shirt? |
;;+-----------+----------------+------------+-------------+--------+
;;A shirt is a (make-shirt (string number string))
;; a material is what the shirt is made of
;; a size is how big or small the shirt is
;; a color is what color the shirt is

;; compare-clothes takes two shirts and returns true or false if they share
;; the same characteristics
;; takes (struct struct) returns boolean
;;(define (compare-clothes-tmpl shirt1 shirt2)
;;  (cond
;;    [(&& (= (... shirt1) (... shirt2) (= ...) (= ...)) ...]
;;    [else ...]))


;;PROBLEM 2
(define-struct time (hours minutes))
; A Time is a structure:
;    (make-time Number Number)
; interpretation: (make-time h m) is the time  
; expressed in hours, and minutes
; Constraints:
; – hours is always between 0 and 11
; – minutes is always between 0 and 59

; t is a struct time
; time-minutes is a number
; time-hours is a number
; tock takes a struct time and returns a new struct representing
; the time 1 minute later (accounting for the fact that the minutes
; cannot exceed 59 and the hours cannot exceed 12)
;(define (tock-templ t)
;  (cond
;    [(= ... t) ...]
;    [ ... t ...]))

(define (tock t)
  (cond
    [(= (time-minutes t) 59)
     (cond
       [(= (time-hours t) 12) (make-time 1 0)]
       [else (make-time (+ 1 (time-hours t)) 0)])]
    [else (make-time (time-hours t) (+ 1 (time-minutes t)))]))

(check-expect (time-minutes (tock (make-time 9 8))) 9)
(check-expect (time-minutes (tock (make-time 12 59))) 0)
(check-expect (time-hours (tock (make-time 12 59))) 1)

; t is a struct time
; time-minutes is a number
; time-hours is a number
; time-to-text takes a struct time and returns a text image of a new string representing
; the time as "hours" + ":" + "minutes". The function will also have to account for
; if the minutes are less than 10 to add a 0
;(define (time-to-text-templ t)
;  (cond
;    [... t ...]
;    [else t ...]))

(define (time-to-text t)
  (cond
    [(> 10 (time-minutes t))
     (text (string-append (number->string (time-hours t)) ":0" (number->string (time-minutes t))) 100 "green")]
    [else (text (string-append (number->string (time-hours t)) ":" (number->string (time-minutes t))) 100 "green")]))


(define (main t)
  (big-bang t
            (to-draw time-to-text)))

;Main Test
;(main (make-time 10 11))

;;PROBLEM 3

;; A ballposn is a (make-ball-posn (number number))
;; An x is the balls position on the x axis
;; A Y is the balls position on the y axis
;;ball-posn-x is a number
;;ball-posn-y is a number
;;make-ball-posn
(define-struct ball-posn (x y))

;; A spe-dir is a (make-spe-dir (number string))
;; A speed is the speed at which the ball moves
;; A direction is the direction in which the ball moves
;; spe-dir-speed is a number
;; spe-dir-direction is a string
;; make-spe-dir
(define-struct spe-dir (speed direction))

;; A ball is a (make-ball (struct struct))
;; A ballposn is the current position of the ball
;; A spedir is the current speed and direction of the ball
;; ball-ballposn is a struct ball-posn
;; ball-spedir is a struct spe-dir
;; make-ball
(define-struct ball (ballposn spedir))

;; ball-next takes a ball and returns a new ball representing the
;; ball's new location depending on its speed and direction
;; A b is a struct ball
;;(define (ball-next-tmpl b)
;;  (cond
;;    [(... b ...)] ...)

(define (ball-next b)
  (cond
    [(string=? (spe-dir-direction (ball-spedir b)) "up")
     (make-ball (make-ball-posn (ball-posn-x (ball-ballposn b))
                                (- (ball-posn-y (ball-ballposn b)) (spe-dir-speed (ball-spedir b))))
                (ball-spedir b))]
    [(string=? (spe-dir-direction (ball-spedir b)) "down")
     (make-ball (make-ball-posn (ball-posn-x (ball-ballposn b))
                                (+ (ball-posn-y (ball-ballposn b)) (spe-dir-speed (ball-spedir b))))
                (ball-spedir b))]
    [(string=? (spe-dir-direction (ball-spedir b)) "right")
     (make-ball (make-ball-posn (+ (ball-posn-x (ball-ballposn b)) (spe-dir-speed (ball-spedir b)))
                                (ball-posn-y (ball-ballposn b)))
                (ball-spedir b))]
    [(string=? (spe-dir-direction (ball-spedir b)) "left")
     (make-ball (make-ball-posn (- (ball-posn-x (ball-ballposn b)) (spe-dir-speed (ball-spedir b)))
                                (ball-posn-y (ball-ballposn b)))
                (ball-spedir b))]
    [else b]))

(check-expect (ball-posn-x (ball-ballposn (ball-next (make-ball (make-ball-posn 0 0) (make-spe-dir 10 "right"))))) 10)
;; ball-image takes a ball and returns an image of the ball
;; on an empty background at the ball's coordinates ballposn
;; A b is a struct ball
;;(define (ball-image-tmpl b)
;;  (place-image
;;   (circle ...)
;;   (... b ...)
;;   (empty-scene ...)))

(define (ball-image b)
  (place-image
   (circle 10 "solid" "red")
   (ball-posn-x (ball-ballposn b)) (ball-posn-y (ball-ballposn b))
   (empty-scene 300 300)))

;; ball-change takes a ball and a key event and returns
;; a new ball with its spe-dir-direction updated to the key event
;; (the key event must be one of the arrow keys for this
;; to occur)
;; A b is a struct ball
;; A key-ev is a key event
;;(define (ball-change-tmpl b key-ev)
;;(cond
;;    [(or (... key-ev ..))
;;     (make-ball (... b ...))]
;;    [else b]))

(define (ball-change b key-ev)
  (cond
    [(or (string=? key-ev "up") (string=? key-ev "down") (string=? key-ev "left") (string=? key-ev "right"))
     (make-ball (ball-ballposn b) (make-spe-dir 10 key-ev))]
    [else b]))

(define (mainb b)
  (big-bang b
           (to-draw ball-image)
           (on-tick ball-next)
           (on-key ball-change)))

;Main Test
;(mainb (make-ball (make-ball-posn 150 150) (make-spe-dir 0 "none")))