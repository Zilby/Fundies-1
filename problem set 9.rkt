;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |problem set 9|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;Homework 9
;;Alexander Zilbersher
;;Jialin Zhou

;; A BTN is one of
;; - Number
;; - (make-node BTN BTN)
(define-struct node (left right))

;Problem 1
#|Design a function, btn-height, that takes in a binary tree of numbers
and computes the maximum distance from the root to any leaf. Here
"distance" is measured by adding 1 for each internal node on the
way from the root to the leaf.|#

;btn-height takes a BTN (either a Number or a node) b and returns
;a Number indicating the maximum distance from the root to any
;of its leaves (also known as the 'height')
;Number, node -> Number
(define (btn-height b)
  (cond
    [(number? b) 0]
    [else (btn-height-helper (+ 1 (btn-height (node-left b)))
                             (+ 1 (btn-height (node-right b))))]))

(define (btn-height-helper n1 n2)
  (cond
    [(> n1 n2) n1]
    [else n2]))

(check-expect (btn-height 42) 0)
(check-expect (btn-height (make-node 2 (make-node 4 9))) 2)
(check-expect (btn-height (make-node
                           (make-node
                            (make-node
                             (make-node 3 9) 6)
                            (make-node
                             (make-node 4 8)
                             (make-node 7 3)))
                           (make-node 4
                                      (make-node 9 3)))) 4)


;Problem 2
;Design a function, btn-sum, that takes in a binary tree of
;numbers and computes the sum of all leaves.

;btn-sum takes a BTN (either a Number or a node) b and returns
;a Number indicating the total sum of the binary tree's leaves
;Number, node -> Number
(define (btn-sum b)
  (cond
    [(number? b) b]
    [else (+ (btn-sum (node-left b)) (btn-sum (node-right b)))]))

(check-expect (btn-sum 42) 42)
(check-expect (btn-sum (make-node 2 (make-node 4 9))) 15)
(check-expect (btn-sum (make-node
                        (make-node
                         (make-node
                          (make-node 3 9) 6)
                         (make-node
                          (make-node 4 8)
                          (make-node 7 3)))
                        (make-node 4
                                   (make-node 9 3))))
              (+ 3 9 6 4 8 7 3 4 9 3))
;Problem 3
#|A leafy binary tree is a binary tree with the symbol 'leaf
at its leafs (as opposed to, say, a number).
Design a function that consumes a natural number n and creates
(a list of) all the leafy binary trees that have height n.
Hint: Design a function that consumes a natural number n, and
creates (a list of) all leafy binary trees of height equal
or less than n.
Hint: If you have to use filter to remove things from the
list, you’re working too hard.
Hint: It might be helpful to draw pictures.|#

;make-possible-trees takes a Number n and returns a LoLBT
;(list of leafy binary trees) of all leafy binary trees
;that have height n
;Number -> LoLBT
(define (make-possible-trees n)
  (cond
    [(= n 0) (list 'leaf)]
    [(= n 1) (list (make-node 'leaf 'leaf))]
    [else (append (make-right-branches (make-node 'leaf (- n 1)))
                  (make-left-branches (make-node (- n 1) 'leaf)))]))

;make-right-branches takes a node n and returns a LoLBT
;(list of leafy binary trees) of all leafy binary trees
;that have height (node-right n) and the same initial
;(node-left n) value for the first node 
;Node -> LoLBT
(define (make-right-branches n)
  (cond
    [(= (node-right n) 0) (list (make-node 'leaf 'leaf))]
    [(= (node-right n) 1) (list (make-node 'leaf (make-node 'leaf 'leaf)))]
    [else (map (lambda (x) (make-node 'leaf x))
               (make-possible-trees (node-right n)))]))

;make-left-branches takes a node n and returns a LoLBT
;(list of leafy binary trees) of all leafy binary trees
;that have height (node-left n) and the same initial
;(node-right n) value for the first node 
;Node -> LoLBT
(define (make-left-branches n)
  (cond
    [(= (node-left n) 0) (list (make-node 'leaf 'leaf))]
    [(= (node-left n) 1) (list (make-node (make-node 'leaf 'leaf) 'leaf))]
    [else (map (lambda (x) (make-node x 'leaf))
               (make-possible-trees (node-left n)))]))

(check-expect (make-right-branches (make-node 'leaf 0))
              (list (make-node 'leaf 'leaf)))
(check-expect (make-right-branches (make-node 'leaf 3))
              (list
               (make-node 'leaf (make-node 'leaf (make-node 'leaf (make-node 'leaf 'leaf))))
               (make-node 'leaf (make-node 'leaf (make-node (make-node 'leaf 'leaf) 'leaf)))
               (make-node 'leaf (make-node (make-node 'leaf (make-node 'leaf 'leaf)) 'leaf))
               (make-node 'leaf (make-node (make-node (make-node 'leaf 'leaf) 'leaf) 'leaf))))

(check-expect (make-left-branches (make-node 0 'leaf))
              (list (make-node 'leaf 'leaf)))
(check-expect (make-left-branches (make-node 3 'leaf))
              (list
               (make-node (make-node 'leaf (make-node 'leaf (make-node 'leaf 'leaf))) 'leaf)
               (make-node (make-node 'leaf (make-node (make-node 'leaf 'leaf) 'leaf)) 'leaf)
               (make-node (make-node (make-node 'leaf (make-node 'leaf 'leaf)) 'leaf) 'leaf)
               (make-node (make-node (make-node (make-node 'leaf 'leaf) 'leaf) 'leaf) 'leaf)))
              
(check-expect (make-possible-trees 0)
              (list 'leaf))
(check-expect (make-possible-trees 1)
              (list (make-node 'leaf 'leaf)))
(check-expect (make-possible-trees 2)
              (list (make-node 'leaf (make-node 'leaf 'leaf))
                    (make-node (make-node 'leaf 'leaf) 'leaf)))
(check-expect (make-possible-trees 3)
              (list
               (make-node 'leaf (make-node 'leaf (make-node 'leaf 'leaf)))
               (make-node 'leaf (make-node (make-node 'leaf 'leaf) 'leaf))
               (make-node (make-node 'leaf (make-node 'leaf 'leaf)) 'leaf)
               (make-node (make-node (make-node 'leaf 'leaf) 'leaf) 'leaf)))
(check-expect (make-possible-trees 4)
              (list
               (make-node 'leaf (make-node 'leaf (make-node 'leaf (make-node 'leaf 'leaf))))
               (make-node 'leaf (make-node 'leaf (make-node (make-node 'leaf 'leaf) 'leaf)))
               (make-node 'leaf (make-node (make-node 'leaf (make-node 'leaf 'leaf)) 'leaf))
               (make-node 'leaf (make-node (make-node (make-node 'leaf 'leaf) 'leaf) 'leaf))
               (make-node (make-node 'leaf (make-node 'leaf (make-node 'leaf 'leaf))) 'leaf)
               (make-node (make-node 'leaf (make-node (make-node 'leaf 'leaf) 'leaf)) 'leaf)
               (make-node (make-node (make-node 'leaf (make-node 'leaf 'leaf)) 'leaf) 'leaf)
               (make-node (make-node (make-node (make-node 'leaf 'leaf) 'leaf) 'leaf) 'leaf)))