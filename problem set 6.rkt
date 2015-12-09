;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |problem set 6|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;Homework 6
;;Alexander Zilbersher
;;Jialin Zhou

(require 2htdp/image)
(require 2htdp/universe)

(define width 100) ;width of scene
(define height 200) ;height of scene 
(define scene (empty-scene width height)) ;canvas

;a single tetrimino, b1-4 are the blocks, pivot is the pivot point, color is its color
(define-struct tetra (b1 b2 b3 b4 pivot color)) 

;makes a new tetramino determining its color and shape from a random number 0-6
(define (new-tetra color)
  (cond
    [(= 0 color) (make-tetra (make-posn 45 5) (make-posn 55 5) (make-posn 45 15) (make-posn 55 15) (make-posn 50 10) "green")]
    [(= 1 color) (make-tetra (make-posn 35 5) (make-posn 45 5) (make-posn 55 5) (make-posn 65 5) (make-posn 50 10) "blue")]
    [(= 2 color) (make-tetra (make-posn 45 15) (make-posn 55 15) (make-posn 65 15) (make-posn 65 5) (make-posn 60 10) "purple")]
    [(= 3 color) (make-tetra (make-posn 45 5) (make-posn 45 15) (make-posn 55 15) (make-posn 65 15) (make-posn 50 10) "cyan")]
    [(= 4 color) (make-tetra (make-posn 45 15) (make-posn 55 5) (make-posn 55 15) (make-posn 65 15) (make-posn 55 15) "orange")]
    [(= 5 color) (make-tetra (make-posn 45 5) (make-posn 55 5) (make-posn 55 15) (make-posn 65 15) (make-posn 50 10) "pink")]
    [(= 6 color) (make-tetra (make-posn 45 15) (make-posn 55 15) (make-posn 55 5) (make-posn 65 5) (make-posn 50 10) "red")]))

;all the tetriminos on the screen
;tetra is the currently active tetrimino
;pile is all of the currently placed tetriminos
(define-struct world (tetra pile))

;draws the currently active tetriminos and the pile
(define (draw-world w)
  (cond
    [(world? w)
     (place-images
      (list(square 10 "solid" (tetra-color (world-tetra w)))
           (square 10 "solid" (tetra-color (world-tetra w)))
           (square 10 "solid" (tetra-color (world-tetra w)))
           (square 10 "solid" (tetra-color (world-tetra w))))
      (list(tetra-b1 (world-tetra w)) 
           (tetra-b2 (world-tetra w)) 
           (tetra-b3 (world-tetra w))
           (tetra-b4 (world-tetra w)))
      (cond [(empty? (world-pile w)) scene]
            [else (draw-world (world-pile w))]))]
    [(empty? w) scene]
    [else (place-images
           (list (square 10 "solid" (tetra-color (first w)))
                 (square 10 "solid" (tetra-color (first w)))
                 (square 10 "solid" (tetra-color (first w)))
                 (square 10 "solid" (tetra-color (first w))))
           (list(tetra-b1 (first w)) 
                (tetra-b2 (first w)) 
                (tetra-b3 (first w))
                (tetra-b4 (first w)))
           (draw-world (rest w)))]))

;determines whether a tetrimino is at the bottom of the screen
(define (at-bottom t)
  (or (>= (posn-y (tetra-b1 t)) (- height 5))
      (>= (posn-y (tetra-b2 t)) (- height 5))
      (>= (posn-y (tetra-b3 t)) (- height 5))
      (>= (posn-y (tetra-b4 t)) (- height 5))))

;determines whether a tetrimino is at the right of the screen
(define (at-right t)
  (or (>= (posn-x (tetra-b1 t)) (- width 5))
      (>= (posn-x (tetra-b2 t)) (- width 5))
      (>= (posn-x (tetra-b3 t)) (- width 5))
      (>= (posn-x (tetra-b4 t)) (- width 5))))

;determines whether a tetrimino is at the left of the screen
(define (at-left t)
  (or (<= (posn-x (tetra-b1 t)) 5)
      (<= (posn-x (tetra-b2 t)) 5)
      (<= (posn-x (tetra-b3 t)) 5)
      (<= (posn-x (tetra-b4 t)) 5)))

;determines whether a tetrimino is too close to the right side
;for a rotation
(define (at-right-r t)
  (or (>= (posn-x (tetra-b1 t)) (+ width 5))
      (>= (posn-x (tetra-b2 t)) (+ width 5))
      (>= (posn-x (tetra-b3 t)) (+ width 5))
      (>= (posn-x (tetra-b4 t)) (+ width 5))))

;determines whether a tetrimino is too close to the left side
;for a rotation
(define (at-left-r t)
  (or (<= (posn-x (tetra-b1 t)) -5)
      (<= (posn-x (tetra-b2 t)) -5)
      (<= (posn-x (tetra-b3 t)) -5)
      (<= (posn-x (tetra-b4 t)) -5)))

;determines whether a tetrimino is too close to the bottom
;for a rotation
(define (at-bottom-r t)
  (or (>= (posn-y (tetra-b1 t)) (+ height 5))
      (>= (posn-y (tetra-b2 t)) (+ height 5))
      (>= (posn-y (tetra-b3 t)) (+ height 5))
      (>= (posn-y (tetra-b4 t)) (+ height 5))))

;determines whether a tetrimino is below the currently active tetrimino
(define (touching-down w)
    (touching-helper (list (make-posn (posn-x (tetra-b1 (world-tetra w))) (+ 10 (posn-y (tetra-b1 (world-tetra w)))))
                           (make-posn (posn-x (tetra-b2 (world-tetra w))) (+ 10 (posn-y (tetra-b2 (world-tetra w)))))
                           (make-posn (posn-x (tetra-b3 (world-tetra w))) (+ 10 (posn-y (tetra-b3 (world-tetra w)))))
                           (make-posn (posn-x (tetra-b4 (world-tetra w))) (+ 10 (posn-y (tetra-b4 (world-tetra w))))))
                     (get-centers (world-pile w))))

;determines whether a tetrimino is to the right of the currently active tetrimino
(define (touching-right w)
    (touching-helper (list (make-posn (+ 10 (posn-x (tetra-b1 (world-tetra w)))) (posn-y (tetra-b1 (world-tetra w))))
                           (make-posn (+ 10 (posn-x (tetra-b2 (world-tetra w)))) (posn-y (tetra-b2 (world-tetra w))))
                           (make-posn (+ 10 (posn-x (tetra-b3 (world-tetra w)))) (posn-y (tetra-b3 (world-tetra w))))
                           (make-posn (+ 10 (posn-x (tetra-b4 (world-tetra w)))) (posn-y (tetra-b4 (world-tetra w)))))
                     (get-centers (world-pile w))))

;determines whether a tetrimino is to the left of the currently active tetrimino
(define (touching-left w)
    (touching-helper (list (make-posn (- (posn-x (tetra-b1 (world-tetra w)))10) (posn-y (tetra-b1 (world-tetra w))))
                           (make-posn (- (posn-x (tetra-b2 (world-tetra w)))10) (posn-y (tetra-b2 (world-tetra w))))
                           (make-posn (- (posn-x (tetra-b3 (world-tetra w)))10) (posn-y (tetra-b3 (world-tetra w))))
                           (make-posn (- (posn-x (tetra-b4 (world-tetra w)))10) (posn-y (tetra-b4 (world-tetra w)))))
                     (get-centers (world-pile w))))

;gets the coordinates of the blocks from each of the tetriminos in the pile
(define (get-centers pile)
  (cond
    [(empty? pile) empty]
    [else (cons (tetra-b1 (first pile)) (cons (tetra-b2 (first pile))
                (cons (tetra-b3 (first pile)) (cons (tetra-b4 (first pile))
                (get-centers (rest pile))))))]))

;(get-centers (list (new-tetra (random 7)) (new-tetra (random 7)) (new-tetra (random 7)) (new-tetra (random 7)) (new-tetra (random 7))))

;iterates through lists of centers t and p and checks if any posns of the lists are equal 
(define (touching-helper t p)
  (cond
    [(posn? t)
     (cond
       [(empty? p) false]
       [(equal? t (first p)) true]
       [else (touching-helper t (rest p))])]
    [(empty? t) false]
    [else (or (touching-helper (first t) p) (touching-helper (rest t) p))]))

;moves the currently active tetrimino downwards, if it can
;if the tetriminos are at the top it will restart the game
(define (fall w)
  (cond
    [(or(at-bottom (world-tetra w))(touching-down w))
     (cond
       [(> 20 (posn-y(tetra-pivot(world-tetra w)))) (make-world (new-tetra (random 7)) '())]
       [else (make-world (new-tetra (random 7)) (cons (world-tetra w) (world-pile w)))])]
    [else
     (make-world (make-tetra (make-posn (posn-x (tetra-b1 (world-tetra w))) (+ 10 (posn-y (tetra-b1 (world-tetra w)))))
                             (make-posn (posn-x (tetra-b2 (world-tetra w))) (+ 10 (posn-y (tetra-b2 (world-tetra w)))))
                             (make-posn (posn-x (tetra-b3 (world-tetra w))) (+ 10 (posn-y (tetra-b3 (world-tetra w)))))
                             (make-posn (posn-x (tetra-b4 (world-tetra w))) (+ 10 (posn-y (tetra-b4 (world-tetra w)))))
                             (make-posn (posn-x (tetra-pivot (world-tetra w))) (+ 10 (posn-y (tetra-pivot (world-tetra w)))))
                             (tetra-color (world-tetra w)))
                 (world-pile w))]))

;moves the tetrimino to the left and right and rotates the tetrimino if it can
(define (move w key-ev)
  (cond
    [(string=? key-ev "left")
     (cond
       [(or (touching-left w) (at-left (world-tetra w))) w]
       [else (make-world (make-tetra (make-posn (- (posn-x (tetra-b1 (world-tetra w)))10) (posn-y (tetra-b1 (world-tetra w))))
                                     (make-posn (- (posn-x (tetra-b2 (world-tetra w)))10) (posn-y (tetra-b2 (world-tetra w))))
                                     (make-posn (- (posn-x (tetra-b3 (world-tetra w)))10) (posn-y (tetra-b3 (world-tetra w))))
                                     (make-posn (- (posn-x (tetra-b4 (world-tetra w)))10) (posn-y (tetra-b4 (world-tetra w))))
                                     (make-posn (- (posn-x (tetra-pivot (world-tetra w)))10) (posn-y (tetra-pivot (world-tetra w))))
                                     (tetra-color (world-tetra w)))
                         (world-pile w))])]
    [(string=? key-ev "right") 
     (cond
       [(or (touching-right w) (at-right (world-tetra w))) w]
       [else (make-world (make-tetra (make-posn (+ 10 (posn-x (tetra-b1 (world-tetra w)))) (posn-y (tetra-b1 (world-tetra w))))
                                     (make-posn (+ 10 (posn-x (tetra-b2 (world-tetra w)))) (posn-y (tetra-b2 (world-tetra w))))
                                     (make-posn (+ 10 (posn-x (tetra-b3 (world-tetra w)))) (posn-y (tetra-b3 (world-tetra w))))
                                     (make-posn (+ 10 (posn-x (tetra-b4 (world-tetra w)))) (posn-y (tetra-b4 (world-tetra w))))
                                     (make-posn (+ 10 (posn-x (tetra-pivot (world-tetra w)))) (posn-y (tetra-pivot (world-tetra w))))
                                     (tetra-color (world-tetra w)))
                         (world-pile w))])]
    [(string=? key-ev "a") 
     (cond
       [(or (touching-rotate-ccw w) 
            (at-right-r (tetra-rotate-ccw (world-tetra w)))
            (at-left-r (tetra-rotate-ccw (world-tetra w)))
            (at-bottom-r (tetra-rotate-ccw (world-tetra w)))) w]
       [else (make-world (tetra-rotate-ccw (world-tetra w))
                         (world-pile w))])]
    [(string=? key-ev "s") 
     (cond
       [(or (touching-rotate-cw w) 
            (at-right-r (tetra-rotate-cw (world-tetra w)))
            (at-left-r (tetra-rotate-cw (world-tetra w)))
            (at-bottom-r (tetra-rotate-cw (world-tetra w)))) w]
       [else (make-world (tetra-rotate-cw (world-tetra w))
                         (world-pile w))])]
    [else w]))

;rotates the individual blocks of a tetrimino
(define (posn-rotate-ccw p pivot)
  (make-posn (+ (posn-x pivot)
                 (- (posn-y pivot)
                    (posn-y p)))
              (+ (posn-y pivot)
                 (- (posn-x p)
                    (posn-x pivot)))))

;rotates the whole tetrimino ccw
(define (tetra-rotate-ccw t)
  (make-tetra
   (posn-rotate-ccw (tetra-b1 t) (tetra-pivot t))
   (posn-rotate-ccw (tetra-b2 t) (tetra-pivot t))
   (posn-rotate-ccw (tetra-b3 t) (tetra-pivot t))
   (posn-rotate-ccw (tetra-b4 t) (tetra-pivot t))
   (tetra-pivot t)
   (tetra-color t)))

;rotates the whole tetrimino cw
(define (tetra-rotate-cw t)
  (tetra-rotate-ccw (tetra-rotate-ccw (tetra-rotate-ccw t))))

;determines whether another tetrimino will interfere with rotating ccw
(define (touching-rotate-ccw w)
    (touching-helper (list (tetra-b1 (tetra-rotate-ccw (world-tetra w)))
                           (tetra-b2 (tetra-rotate-ccw (world-tetra w)))
                           (tetra-b3 (tetra-rotate-ccw (world-tetra w)))
                           (tetra-b4 (tetra-rotate-ccw (world-tetra w))))
                     (get-centers (world-pile w))))

;determines whether another tetrimino will interfere with rotating cw
(define (touching-rotate-cw w)
    (touching-helper (list (tetra-b1 (tetra-rotate-cw (world-tetra w)))
                           (tetra-b2 (tetra-rotate-cw (world-tetra w)))
                           (tetra-b3 (tetra-rotate-cw (world-tetra w)))
                           (tetra-b4 (tetra-rotate-cw (world-tetra w))))
                     (get-centers (world-pile w))))

(define tetris (make-world (new-tetra (random 7)) empty))

(define (main w)
  (big-bang w
    [to-draw draw-world]
    [on-tick fall .2]
    [on-key move]
    ))

(main tetris)