;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |problem set 10|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;Homework 11
;;Alexander Zilbersher
;;Jialin Zhou

;Problem 1

(define (prime? x)
  (local ((define (prime-A x a)
            (cond
              [(<= a 1) true]
              [(= (modulo x a) 0) false]
              [else (prime-A x (- a 1))])))
    (prime-A x (floor (integer-sqrt x)))))


(check-expect (prime? 17) true)
(check-expect (prime? 23) true)
(check-expect (prime? 9) false)
(check-expect (prime? 143) false)

(define (list-primes x)
  (local ((define (list-primes-a x l)
            (cond
              [(<= x 1) l]
              [(prime? x) (list-primes-a (- x 1)
                                         (cons x l))]
              [else (list-primes-a (- x 1) l)])))
    (list-primes-a x '())))

(check-expect(list-primes 10)
             (list 2 3 5 7))
(check-expect(list-primes 50)
             (list 2 3 5 7 11 13 17 19
                   23 29 31 37 41 43 47))
(check-expect(list-primes 100)
             (list 2 3 5 7 11 13 17 19
                   23 29 31 37 41 43 47
                   53 59 61 67 71 73 79
                   83 89 97))

;Problem 2

;Problem 3

;Problem 4