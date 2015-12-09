;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |problem set 11|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;Homework 11
;;Alexander Zilbersher
;;Jialin Zhou

;Problem 1

;(prime? n) takes a Number n and returns a
;Boolean indicating whether that number is
;a prime number
;Number -> Boolean
(define (prime? n)
  (local (;(prime-A n a) goes through all numbers
          ;0-a and returns a Boolean
          ;indicating if n is divisible
          ;by any numbers in that set
          ;Number, Number -> Boolean
          (define (prime-A n a)
            (cond
              [(<= a 1) true]
              [(= (modulo n a) 0) false]
              [else (prime-A n (- a 1))])))
    (prime-A n (floor (integer-sqrt n)))))


(check-expect (prime? 17) true)
(check-expect (prime? 23) true)
(check-expect (prime? 9) false)
(check-expect (prime? 143) false)

;(list-primes n) takes a Number n and returns a
;list of all prime numbers less than or equal to n
;Number -> [List of Numbers]
(define (list-primes n)
  (local (;(list-primes-a a l) goes through
          ;all numbers 0-a and returns a list
          ;of all the numbers that are determined
          ;to be prime numbers
          ;Number, [List of Numbers] -> [List of Numbers]
          (define (list-primes-a a l)
            (cond
              [(<= a 1) l]
              [(prime? a) (list-primes-a (- a 1)
                                         (cons a l))]
              [else (list-primes-a (- a 1) l)])))
    (list-primes-a n '())))

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

;(fibonacci n) takes a Number n and returns
;the corresponding number in the fibonacci sequence
;Number -> Number
(define (fibonacci n)
  (cond
    [(= n 0) 0]
    [(= n 1) 1]
    [else (+ (fibonacci (- n 1))
             (fibonacci (- n 2)))]))

(check-expect (fibonacci 0) 0)
(check-expect (fibonacci 1) 1)
(check-expect (fibonacci 2) 1)
(check-expect (fibonacci 5) 5)
(check-expect (fibonacci 8) 21)
(check-expect (fibonacci 12) 144)
(check-expect (fibonacci 15) 610)

;(fibonacci2 n) takes a Number n and returns
;the corresponding number in the fibonacci sequence
;using an accumulator
;Number -> Number
(define (fibonacci2 n)
  (local (;(fib-a n1 n2 a) takes three numbers n1, n2 and
          ;a and recurses switching n1 for n2 and n2
          ;for n1+n2 until it reduces a to 0
          ;Number, Number, Number -> Number
          (define (fib-a n1 n2 a)
            (cond
              [(= a 0) n1]
              [else (fib-a n2 (+ n1 n2) (- a 1))])))
          (fib-a 0 1 n)))

(check-expect (fibonacci2 0) 0)
(check-expect (fibonacci2 1) 1)
(check-expect (fibonacci2 2) 1)
(check-expect (fibonacci2 5) 5)
(check-expect (fibonacci2 8) 21)
(check-expect (fibonacci2 12) 144)
(check-expect (fibonacci2 15) 610)

;(list-fibonacci n) takes a Number n and returns a
;list of the first n numbers of the fibonacci sequence
;Number -> [List of Numbers]
(define (list-fibonacci n)
  (local (;(list-fibonacci-a a l) recursively inserts
          ;numbers of the fibonacci sequence into l until
          ;a is less than 0
          ;Number, [List of Numbers] -> [List of Numbers]
          (define (list-fibonacci-a a l)
            (cond
              [(< a 0) l]
              [else (list-fibonacci-a
                     (- a 1) (cons (fibonacci2 a) l))])))
    (list-fibonacci-a n '())))

(check-expect (list-fibonacci 5) (list 0 1 1 2 3 5))
(check-expect (list-fibonacci 8) (list 0 1 1 2 3 5 8 13 21))
(check-expect (list-fibonacci 12) (list 0 1 1 2 3 5 8 13
                                        21 34 55 89 144))
(check-expect (list-fibonacci 15) (list 0 1 1 2 3 5 8 13 21
                                        34 55 89 144 233 377 610))

;Problem 3

;;; Syntax -- the grammar of Husky, as Racket sexpressions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; A HExp (Husky Expression) is one of:
;;;   The Data                                  ; How it looks as an s-exp
;;;   --------                                  ; ------------------------
;;; - Number                                    ; <number>
;;; - Var                                       ; <var>
;;; - (list 'const SExp)                        ; (const <constant-sexpression>)
;;; - (list 'fun (list Var ...) HExp)           ; (fun (<var> ...) <body>)
;;; - (list 'if HExp HExp HExp)                 ; (if <test> <then> <else>)
;;; - (list HExp HExp ...)                      ; (<fun> <arg> <arg> ...)
;;; - (list 'and HExp HExp ...)                 ; (and <arg> <arg> ...)
;;; - (list 'or HExp HExp ...)                  ; (or <arg> <arg> ...)
;;;
;;; A Var is a Symbol.

;;; Semantics -- semantic values, environments and the interpreter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A Value is one of:
;;; - SExp              (a number, boolean, cons, etc.)
;;; - Procedure
;;;
;;; A Procedure is one of:
;;; - (make-closure [List-of Var] HExp Env)     (a user procedure)
;;; - (make-primop [[List-of Value] -> Value])  (a built-in procedure)
;;;
;;; Closures are what we get when we evaluate (fun (<var> ...) <body>) terms
;;; in some given environment -- we package up the parameters (<var> ...),
;;; the function's <body> expression, and the current environment into a
;;; closure.
;;;
;;; Primops represent "built-in" primitive operations that the interpreter does
;;; directly. Every primop comes with a handler that we use to do the primop.

(define-struct closure (params body env))
(define-struct primop  (handler))

;;; Env = [Listof (make-binding Var Value)]
(define-struct binding (var val))
;;; An environment represents a set of variable/value bindings.

;;; lookup: Env Var -> Val
;;; Look up the variable's value in the environment.
;;; Environments are scanned left to right, so consing a binding for variable
;;; V onto the front of a list shadows any other bindings of V further down
;;; in the environment.
(define (lookup env var)
  (cond [(empty? env) (error 'lookup "Variable is not bound: " var)]
        [else (local [(define b (first env))]
                (cond [(symbol=? (binding-var b) var)
                       (binding-val b)]
                      [else (lookup (rest env) var)]))]))

;;; Eval & Apply -- the yin/yang pair that make the interpreter.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Symbol SExp -> Boolean
;;; Is the s-expression the given keyword?
(define (keyword=? kwd sexp)                    ; Cheesy little syntax
  (and (symbol? sexp)                           ; utility function.
       (symbol=? kwd sexp)))

;; eval : HExp Env -> Value
;; Evaluate the Husky expression in the given environment.
(define (eval exp env)
  (cond [(number? exp) exp]     ; Numeric constants self-evaluate.

        ;; Look up variable references in the current environment.
        [(symbol? exp) (lookup env exp)]

        ;; Expression is a list -- CONST exp, FUN exp, IF exp or function call.
        [(cons? exp)
         (local [(define e1 (first exp))]
           (cond
            ;; (CONST sexp) -- a constant
            [(keyword=? 'const e1) (second exp)]

            ;; (FUN (var ...) body)
            [(keyword=? 'fun e1)
             ;; Make a code+env closure:
             (make-closure (second exp) ; the params
                           (third exp)  ; the body
                           env)]        ; the env

            ;; (IF test then else)
            [(keyword=? 'if e1)
             (eval ((cond [(eval (second exp) env) third ]
                          [else                    fourth]) exp)
                   env)]

            ;**Added these for the problem set**
            
            ;; (AND exp1 exp2 ...)
            [(keyword=? 'and e1)
             (cond [(empty? (rest exp)) true]
                   [(not (eval (second exp) env)) false]
                   [else (eval (cons 'and (rest (rest exp))) env)])]

            ;; (OR exp1 exp2 ...)
            [(keyword=? 'or e1)
             (cond [(empty? (rest exp)) false]
                   [(eval (second exp) env) true]
                   [else (eval (cons 'or (rest (rest exp))) env)])]
            
            ;; Function application: (function arg ...)
            ;; Eval the fun, and all the args, then apply fun to args.
            [else (app (eval (first exp) env)
                       (map (lambda (a) (eval a env))
                            (rest exp)))]))]
        [else (error 'eval "Not a valid Husky expression: " exp)]))

;;; Value [Listof Value] -> Value
;;; Apply the Husky function to the Husky arguments.
(define (app f args)
  (cond [(closure? f)                           ; Proc is a closure:
         (eval (closure-body f)                 ; Eval the function's body
               (append (map make-binding        ; in the closure env extended
                            (closure-params f)  ; with the parameter/argument
                            args)               ; bindings.
                       (closure-env f)))]

        ;; Just hand off to primop's handler.
        [(primop? f) ((primop-handler f) args)]

        [else (error 'app "Attempting to apply a non-function: " f)]))


;; HExp -> Value
;; Run the Husky expression in the top-level environment
(define (run e) (eval e top-env))

(check-expect (run '1)         1)
(check-expect (run '(plus2 5)) 7)
(check-expect (run '(plus2 5)) 7)
(check-expect (run '((fun (x) (minus1 (plus2 x))) 5)) 6)
(check-expect (run '((fun (f) (f (f 0))) plus2)) 4)

;;; Primops & the top-level environment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Convert a Racket/ISL/ASL function into a Husky-interpreter primop.
(define (racket->husky-primop f)
  (make-primop (lambda (args) (apply f args))))

(define top-env
  (list (make-binding 'plus1  (racket->husky-primop add1))
        (make-binding 'minus1 (racket->husky-primop sub1))
        (make-binding 'head   (racket->husky-primop first))
        (make-binding 'tail   (racket->husky-primop rest))
        (make-binding 'list   (racket->husky-primop list))
        (make-binding 'tru    true)
        (make-binding 'fals   false)
        (make-binding 'not    (racket->husky-primop not))
        (make-binding 'plus2  (racket->husky-primop (lambda (n) (+ n 2))))
        (make-binding '*      (racket->husky-primop *))
        (make-binding '+      (racket->husky-primop +))
        (make-binding '-      (racket->husky-primop -))
        (make-binding '=      (racket->husky-primop =))
        (make-binding '<      (racket->husky-primop <))

        ;**Added these for the problem set**
        (make-binding 'substring (racket->husky-primop substring))
        (make-binding 'string-append (racket->husky-primop string-append))
        ))

;and checks__________________________________________________________________

(check-expect (run '((fun (x y) (and y x x)) tru fals)) false)
(check-expect (run '((fun (x y) (and x y x)) tru fals)) false)
(check-expect (run '((fun (x y) (and x x y x x y y x)) tru fals)) false)
(check-expect (run '((fun (x y) (and x x x x x y)) tru fals)) false)
(check-expect (run '((fun (x y) (and y x x)) tru fals)) false)
(check-expect (run '((fun (x y) (and y y y)) tru fals)) false)
(check-expect (run '((fun (x y) (and y x x)) tru fals)) false)
(check-expect (run '((fun (x y) (and x x x)) tru fals)) true)

;or checks___________________________________________________________________

(check-expect (run '((fun (x y) (or y x x)) tru fals)) true)
(check-expect (run '((fun (x y) (or x y x)) tru fals)) true)
(check-expect (run '((fun (x y) (or x x y x x y y x)) tru fals)) true)
(check-expect (run '((fun (x y) (or y y y y y x)) tru fals)) true)
(check-expect (run '((fun (x y) (or y x x)) tru fals)) true)
(check-expect (run '((fun (x y) (or x x x)) tru fals)) true)
(check-expect (run '((fun (x y) (or y x x)) tru fals)) true)
(check-expect (run '((fun (x y) (or y y y)) tru fals)) false)


;substring checks____________________________________________________________

(check-expect (run '(substring (const "Apple") 1 3)) "pp")
(check-expect (run '(substring (const "strings") 0 2)) "st")
(check-expect (run '(substring (const "example") 2 5)) "amp")

;string-append checks____________________________________________________________

(check-expect (run '(string-append (const "Apple")
                                   (const "Orange")))
              "AppleOrange")
(check-expect (run '(string-append (const "papyrus") (const "sans")
                                   (const "arial") (const "times")))
              "papyrussansarialtimes")
(check-expect (run '(string-append (const "This") (const "Is")
                                   (const "The") (const "Final")
                                   (const "Question") (const "Huzzah!")))
              "ThisIsTheFinalQuestionHuzzah!")