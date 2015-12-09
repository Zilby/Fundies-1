;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Lab_5) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;Lab 5

(define (lon-add-n lon n)
  (cond [(empty? lon) empty]
        [else (cons (+ (first lon) n)
                    (lon-add-n (rest lon) n))]))

(define (lon-add2 lon)
  (lon-add-n lon 2))

(define (lon-add5 lon)
  (lon-add-n lon 5))

(check-expect (lon-add-n (list 9 4 38 0) 3) (cons 12 (cons 7 (cons 41 (cons 3 '())))))
(check-expect (lon-add2 (list 9 4 38 0)) (cons 11 (cons 6 (cons 40 (cons 2 '())))))
(check-expect (lon-add5 (list 9 4 38 0)) (cons 14 (cons 9 (cons 43 (cons 5 '())))))

(define (starts-with los prefix)
  (cond
    [(empty? los) empty]
    [(> (string-length prefix) (string-length (first los) )) (starts-with (rest los) prefix)]
    [(string=? (substring (first los) 0 (string-length prefix)) prefix)
     (cons (first los) (starts-with (rest los) prefix))]
    [else (starts-with (rest los) prefix)]))

(check-expect (starts-with (list "oboe" "cataract" "ox" "catharsis") "cat")(list "cataract" "catharsis"))

(define (starts-with-dog los)
  (starts-with los "dog"))

(define (starts-with-cat los)
  (starts-with los "cat"))

; A Nat, or natural number, is one of:
;  - 0
;  - (add1 Nat)
; 
; The predicate for 0 is: zero?
; 
; The predicate for (add1 n): positive?
; The selector for (add1 n): sub1

(define (nat-even? n)
  (cond
    [(zero? n) true]
    [(< (sub1 (sub1 n)) 0) false]
    [else (nat-even? (sub1 (sub1 n)))]))

(check-expect (nat-even? 7) false)
(check-expect (nat-even? 8) true)

(define (double n)
  (cond
    [(= n 0) 0]
    [else(add1 (add1 (double (sub1 n))))]))

(check-expect (double 5) 10)

(define (down-from n)
  (cond
    [(= n 0) (list n)]
    [else (append (list n) (down-from (sub1 n)))]))

(check-expect (down-from 3) (list 3 2 1 0))

(define (repeat s n)
  (cond
    [( = 0 n) empty]
    [else (cons s (repeat s (sub1 n)))]))

(check-expect (repeat "buffalo" 8)
(list "buffalo" "buffalo" "buffalo" "buffalo" "buffalo" "buffalo" "buffalo" "buffalo"))

(define (nat+ n1 n2)
  (cond
    [(= n2 0) n1]
    [else (nat+ (add1 n1) (sub1 n2))]))

(check-expect (nat+ 5 9) 14)

(define (nat* n1 n2)
  (cond
    [(= n2 0) 0]
    [else (nat+ n1 (nat* n1 (sub1 n2)))]))

(check-expect (nat* 5 9) 45)

(define (nat-square n)
  (cond
    [(= n 0) 0]
    [else (nat+ (sub1 (double n)) (nat-square (sub1 n)))]))

(check-expect (nat-square 5) 25)
(check-expect (nat-square 7) 49)

;_____________________________________________________________

(require 2htdp/image)
(require 2htdp/universe)
 
(define width 400)
(define height 400)
(define-struct ring (size center))
; A Ring is a (make-ring Nat Posn)
; where size is the ring's radius
;       center is the x,y coordinate of the ring's center
(define ring1 (make-ring 0
                         (make-posn 50 50)))
(define ring2 (make-ring (add1 0)
                         (make-posn 150 0)))
(define ring3 (make-ring (add1 (add1 0))
                         (make-posn 0 75)))
 
; A World is a [List-of Ring]

(define (grow-ring r)
  (make-ring (+ 1 (ring-size r)) (ring-center r)))

