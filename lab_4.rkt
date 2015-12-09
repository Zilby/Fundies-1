;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lab) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define-struct empty-lon ())
(define-struct cons-lon (left right))
(define (sum-a-lon x)
  (cond
    [(empty-lon? x) 0]
    [else (+ (cons-lon-left x) (sumnation (cons-lon-right x)))]))

(define (sumnation x)
  (cond
    [(empty? x) 0]
    [else (+ (first x) (sumnation (rest x)))]))

(check-expect (sumnation (list 5 8 2 7 41 7 23)) (+ 5 8 2 7 41 7 23))

(define (list-len x)
  (cond
    [(empty? x) 0]
    [else (+ 1 (list-len (rest x)))]))

(check-expect (list-len (list 5 8 2 7 41 7 23)) 7)

(define (average x)
  (/ (sumnation x) (list-len x)))

(check-expect (average (list 5 8 2 7 41 7 23)) (/(+ 5 8 2 7 41 7 23)7))

(define (overlay-all x)
  (cond
    [(empty? x) (empty-scene 200 200)]
    [else (overlay (first x) (overlay-all (rest x)))]))

#|(overlay-all (list (circle 30 "outline" "red")
                   (circle 20 "solid" "blue")
                   (star 40 "solid" "gray")))
|#

(define (sum-all x)
  (cond
    [(empty? x) 0]
    [else (+ (sumnation(first x)) (sum-all (rest x)))]))

(check-expect (sum-all (list (list 5 8 2) (list 7 41 7) (list 23 6 32)))
              (+ 5 8 2 7 41 7 23 6 32))

;_____________________________________________________________________
(define CW 500)
(define CH 500)
(define gaccel 2)

(define-struct ball (x y vx vy))

(define (off-screen b)
  (or (< CW (ball-x b)) (< CH (ball-y b)) (> 0 (ball-x b)) (> 0 (ball-y b))))

(define (move-ball b)
  (make-ball (+ (ball-x b) (ball-vx b))
             (+ (ball-y b) (ball-vy b))
             (ball-vx b) (ball-vy b)))

(define (gravity b)
  (make-ball (ball-x b)
             (ball-y b)
             (ball-vx b)
             (+ gaccel (ball-vy b))))

(define (draw-lob LoB)
  (cond
    [(empty? LoB) (empty-scene CW CH)]
    [else (place-image (circle 5 "solid" "red")
                       (ball-x (first LoB)) (ball-y (first LoB))
                       (draw-lob (rest LoB)))]))

(define (on-screen-balls LoB)
  (cond
    [(empty? LoB) LoB]
    [(off-screen (first LoB)) (on-screen-balls (rest LoB))]
    [else (cons (first LoB) (on-screen-balls (rest LoB)))]))

(define (move-all LoB)
  (cond
    [(empty? LoB) LoB]
    [else (cons (move-ball (first LoB)) (move-all (rest LoB)))]))

(define (apply-gravity LoB)
  (cond
    [(empty? LoB) LoB]
    [else (cons (gravity (first LoB)) (apply-gravity (rest LoB)))]))

(define (new-ball LoB mouse-x mouse-y mouse-ev)
  (cond
    [(string=? mouse-ev "button-down")
     (cond 
       [(= 0 (random 4)) (cons LoB (make-ball mouse-x mouse-y
                                              (random 10) (random 10)))]
       [(= 1 (random 4)) (cons LoB (make-ball mouse-x mouse-y
                                              (* -1 (random 10)) (random 10)))]
       [(= 2 (random 4)) (cons LoB (make-ball mouse-x mouse-y
                                              (random 10) (* -1 (random 10))))]
       [else (cons LoB (make-ball mouse-x mouse-y
                                  (* -1 (random 10)) (* -1 (random 10))))])]
    [else LoB]))

; LoB -> LoB
; move balls, apply gravity, and then filter out those that are off screen
(define (go lob)
   (on-screen-balls (apply-gravity (move-all lob))))

; Ball -> LoB
(define (main b)
  (big-bang (cons b empty) ; <— the world state is a LoB
    [to-draw draw-lob]
    [on-tick go]
    [on-mouse new-ball]
    ))

(main (make-ball 100 100 10 -8))


