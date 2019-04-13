;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders


;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 100)

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))

(define MISSILE (ellipse 5 15 "solid" "red"))


;; ==========================================

;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))






(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))







(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))



;;ListOfInvaders is one of :
;; - empty
;; (cons (make-invader Number Number Number) ListOfInvaders)

(define LOI1 empty)
(define LOI2 (cons I1 empty))
(define LOI3 (cons I2 (cons I1 empty)))
(define LOI4 (cons I3 (cons I2 (cons I1 empty))))

#;
(define (fn-for-loinvader loi)
  (cond [(empty? loi) (...)]
        [else
         (... (fn-for-invader (first loi) )
              (fn-for-loinvader (rest loi) ) )]))

;; Template rules used :
;; One of : 2 cases
;; Atomic distinct value : empty
;; Compound : 2 fields
;; reference : (first loi) is Invader
;; self-reference : (rest loi) Is ListOfInvaders







(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))


;; ListOfMissiles is one of :
;; - empty
;; (cons (make-missile Number Number ) ListOfMissiles)

(define LOM1 empty)
(define LOM2 (cons M1 empty) )
(define LOM3 ( cons M3 (cons M2 ( cons M1 empty) ) ) )

#;
(define (fn-for-lom m)
  (cond [(empty? m) (...)]
        [else
         (... (fn-for-missile (first lom))
              (fn-for-lom (rest lom)))]))

;;Template rules used:
;; One of : 2 cases
;; Atomic distinct value : empty
;; Compound : 2 fields
;; reference : (first lom) is Missile
;; self-reference : (rest lom) is ListOfMissels





(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))

;; ==========================================


;; Functions:

;; Game -> Game
;; start the world with (main (make-game empty empty G0)

(define (main G)
  (big-bang G                            ; Game
            (on-tick   advance-game)     ; Game -> Game
            (to-draw   render-game)      ; Game -> Image
            (stop-when game-over?)       ; Game -> Boolean
            (on-key    change-tank)))      ; Game KeyEvent -> Game

;; Game -> Game
;; produce the next game , moves the invaders , create them , moves missiles and tank .
(check-random (advance-game G0) (make-game (create-invader INVADE-RATE) empty (make-tank (+ (/ WIDTH 2) TANK-SPEED) 1)))
(check-random (advance-game G1) (make-game (create-invader INVADE-RATE) empty (make-tank (+ 50 TANK-SPEED) 1)))

;(define (advance-game s) s) ; stub
(define (advance-game g)
   (control-missiles (make-game (advance-invaders (game-invaders g)) (game-missiles g) (move-tank (game-tank g)))))


;; ListOfInvaders -> ListOfInvaders
;; create the invaders and move them in X , Y directions
(define (advance-invaders loin)
  (cond [(empty? loin) (create-invader INVADE-RATE)] 
        [else (cons (move-invader (first loin))
                   (advance-invaders (rest loin)))]))



;; Number -> ListOfInvader
;; Randomly create invaders 
(check-random (create-invader INVADE-RATE) (cond [ (<= (random INVADE-RATE)(/ INVADE-RATE 50))
                                                   (cons (make-invader (random WIDTH) 0 1) empty)]
                                                 [else empty]))

;(define (create-invader INVADE-RATE) empty) ; stub

(define (create-invader invade-rate)
  (cond [(<= (random invade-rate) (/ INVADE-RATE 60))  (cons (make-invader (random WIDTH) 0 1) empty)]
        [else empty]))




;; Invader -> Invader
;; Moves the invader with it's speed in both X , Y Direction , if it's almost at left or side , change the direction of speed
(check-expect (move-invader (make-invader 150 30 1 )) (make-invader (+ 150 INVADER-X-SPEED) (+ 30 INVADER-Y-SPEED)  1))
(check-expect (move-invader (make-invader 150 30 -1)) (make-invader (- 150 INVADER-X-SPEED) (+ 30 INVADER-Y-SPEED) -1))
(check-expect (move-invader (make-invader (+ WIDTH 1) 40 1)) (make-invader (- (+ WIDTH 1) INVADER-X-SPEED) (+ 40 INVADER-Y-SPEED) -1))
(check-expect (move-invader (make-invader -1 40 -1)) (make-invader (+ -1 INVADER-X-SPEED) (+ 40 INVADER-Y-SPEED) 1))

;(define (move-invader invader) invader) ;stub

(define (move-invader invader)
  (cond [(on-screen? invader) (filtered-move invader)]
        [else
         (change-dir invader)]))

;; Invader -> Boolean
;; Takes the invader and if it's on screen return true or out return false
(check-expect (on-screen? (make-invader 150 30 1)) true)
(check-expect (on-screen? (make-invader -.5 30 1)) false)
(check-expect (on-screen? (make-invader (+ WIDTH 1) 30 1)) false)
;(define (on-screen? invader) false) ;stub
(define (on-screen? invader)
  (if (< 0 (invader-x invader) WIDTH) true false))


;; Invader -> Invader
;; Takes the invader and change it's position due to speed
(check-expect (filtered-move (make-invader 40 50 1)) (make-invader (+ 40 INVADER-X-SPEED) (+ 50 INVADER-Y-SPEED)  1))
(check-expect (filtered-move (make-invader 5 60 -1)) (make-invader (- 5  INVADER-X-SPEED) (+ 60 INVADER-Y-SPEED) -1))

;(define (filtered-move invader) invader) ;stub

(define (filtered-move invader)
  (cond [(< (invader-dx invader) 0) (make-invader ( - (invader-x invader) INVADER-X-SPEED) ( + (invader-y invader) INVADER-Y-SPEED) (invader-dx invader) ) ]
        [(> (invader-dx invader) 0) (make-invader ( + (invader-x invader) INVADER-X-SPEED) ( + (invader-y invader) INVADER-Y-SPEED) (invader-dx invader) ) ]))


;; Invader -> Invader
;; Takes the invader and change it's poisition and the speed direction
(check-expect (change-dir (make-invader -2 50 -1)) (make-invader (+ -2 INVADER-X-SPEED) (+ 50 INVADER-Y-SPEED) 1))
(check-expect (change-dir (make-invader (+ WIDTH 1) 50 1)) (make-invader (- (+ WIDTH 1) INVADER-X-SPEED) (+ 50 INVADER-Y-SPEED) -1))

;(define (change-dir invader) invader) ;stub

(define (change-dir invader)
   (cond [(< (invader-dx invader) 0) (make-invader  ( + (invader-x invader) INVADER-X-SPEED) ( + (invader-y invader) INVADER-Y-SPEED)  (-(invader-dx invader)))]
         [(> (invader-dx invader) 0) (make-invader  ( - (invader-x invader) INVADER-X-SPEED) ( + (invader-y invader) INVADER-Y-SPEED)  (-(invader-dx invader)))]))


;; Game -> Game
;; Takes a game move the missiles in Y direction with the speed missile , if missiles hits invader it demolish the invader
(check-expect (control-missiles G0) G0)
(check-expect (control-missiles G1) G1)
(check-expect (control-missiles G2) (make-game (cons I1 empty)  (cons (make-missile (missile-x M1) (- (missile-y M1) MISSILE-SPEED)) empty) T1))
(check-expect (control-missiles G3) (make-game (cons I2 empty)  (cons (make-missile (missile-x M1) (- (missile-y M1) MISSILE-SPEED)) 
                                                                      (cons (make-missile (missile-x M2) (- (missile-y M2) MISSILE-SPEED)) empty)) T1))
;(define (control-missiles g) g) ;stub
(define (control-missiles g)
  (make-game (filter-invaders (game-invaders g) (game-missiles g))
             (advance-missiles (game-missiles g))
             (game-tank g)))


;; ListOfInvaders ListOfMissiles -> ListOfInvaders 
;; Takes a list of invaders and list of missiles and check if any missile around the hit range of the invader then kills it if it's near it.
(check-expect (filter-invaders empty empty) empty)
(check-expect (filter-invaders LOI2  empty) LOI2)
(check-expect (filter-invaders empty LOM2) empty)
(check-expect (filter-invaders LOI2 LOM2) LOI2)
(check-expect (filter-invaders LOI2 LOM3) LOI1)
(check-expect (filter-invaders LOI3 LOM2) LOI3)
(check-expect (filter-invaders LOI3 LOM3) (cons I2 empty))

; (define (filter-invaders loinvaders lom) loinvaders) ;stub
(define (filter-invaders loi lom)
  (cond [(empty? loi) empty]
        [else
               (cond [(filter-invader (first loi) lom) (cons (first loi) (filter-invaders (rest loi) lom))] 
                     [else
                      (filter-invaders (rest loi) lom) ])]))

;; Invader ListOfMissiles -> Boolean
;; Takes invader and if it's in the hit range of any missile in the missile list , demolish the invader
(check-expect (filter-invader I1 LOM1) true)
(check-expect (filter-invader I1 LOM2) true)
(check-expect (filter-invader I1 LOM3) false)
;(define (filter-invader invader lom) invader) ;stub
(define (filter-invader invader lom)
  (cond [(empty? lom) true ]
        [else
         (if (hit-range?  invader (first lom)) false (filter-invader invader (rest lom)))]))

;; Invader Missile -> Boolean
;; If the invader in the missile hit range it produce true
(check-expect (hit-range? I1  M1) false)
(check-expect (hit-range? I1 M2) true)
(check-expect (hit-range? I1 M3) true)

(define (hit-range? I M)
(and (<= (- HIT-RANGE) (- (invader-x I) (missile-x M) ) HIT-RANGE) (<= (- HIT-RANGE) (- (invader-y I) (missile-y M)) HIT-RANGE) ) )





;; ListOfMissiles -> ListOfMissiles
;; Takes a list of missiles and move them in Y direction by the invasion rate 
(check-expect (advance-missiles LOM1) empty)
(check-expect (advance-missiles LOM2) (cons (make-missile (missile-x M1) (- (missile-y M1) MISSILE-SPEED)) empty))
(check-expect (advance-missiles LOM3) (cons (make-missile (missile-x M3) (- (missile-y M3) MISSILE-SPEED)) (cons (make-missile (missile-x M2) (- (missile-y M2) MISSILE-SPEED))
                                                                                                                 (cons (make-missile (missile-x M1) ( - (missile-y M1) MISSILE-SPEED))
                                                                                                                       empty))))
; (define (advance-missiles lom) lom) ;stub

(define (advance-missiles lom)
  (cond [(empty? lom) empty]
        [else
         (cons  (move-missile     (first lom))
                (advance-missiles (rest lom)))]))  

;; Missile -> Missile
;; Takes one missile and move it in Y direction by the missile speed
(check-expect (move-missile (make-missile 20 30)) (make-missile 20 (- 30 MISSILE-SPEED)))
(check-expect (move-missile (make-missile 20 1)) (make-missile 20  (- 1  MISSILE-SPEED)))

; (define (move-missile m) m) ;stub
(define (move-missile m)
  (make-missile (missile-x m) (- (missile-y m) MISSILE-SPEED)))


;; Tank -> Tank
;; Takes a tank and move it in it's direction .
(check-expect (move-tank (make-tank 150  1) ) (make-tank  ( + 150 TANK-SPEED)  1))
(check-expect (move-tank (make-tank 150 -1) ) (make-tank  ( - 150 TANK-SPEED) -1))
(check-expect (move-tank (make-tank WIDTH 1) ) (make-tank (- WIDTH 1) -1))
(check-expect (move-tank (make-tank 0 -1 )) (make-tank 1 1))

;; (define (move-tank t) t) ;stub

(define (move-tank t)
  (if (on-range? (tank-x t)) (progress-tank t) (change-tank-dir t)))


;; Number -> Boolean
;; Takes the position of the tank , if on the range of screen return true , if on borders or out the screen return false.

(check-expect (on-range? WIDTH) false)
(check-expect (on-range? 150) true)
(check-expect (on-range? 0) false)

;(define (on-range? n) false) ;stub

(define (on-range? n)
  (if (< 0 n WIDTH) true false))


;; Tank -> Tank
;; Takes a tank and moves it in the right if dir = 1 , if dir = -1 moves to left
(check-expect (progress-tank (make-tank 150 1)) (make-tank (+ 150 TANK-SPEED) 1))
(check-expect (progress-tank (make-tank 150 -1)) (make-tank (- 150 TANK-SPEED) -1))
; (define (progress-tank t) t)

(define (progress-tank t)
  (make-tank (cond [( > (tank-dir t) 0 ) (+ (tank-x t) TANK-SPEED) ]
                   [( < (tank-dir t) 0 ) (- (tank-x t) TANK-SPEED) ]) (tank-dir t)))

;; Tank -> Tank
;; Changes the tank direction
(check-expect (change-tank-dir (make-tank 0 -1)) (make-tank 1 1))
(check-expect (change-tank-dir (make-tank WIDTH 1)) (make-tank (- WIDTH 1) -1))
;(define (change-tank-dir t) t) ;stub
(define (change-tank-dir t)
  (cond [(<= (tank-x t) 0) (make-tank 1 1)]
        [else
         (make-tank (- WIDTH 1) -1)]))





;; Game -> Image
;; render the game , put the whole image of the invaders , the tank & the missiles .
(check-expect (render-game G0) (place-image TANK (tank-x T0) (- HEIGHT (/ (image-height TANK) 2)) BACKGROUND))
(check-expect (render-game G1) (place-image TANK (tank-x T1) (- HEIGHT (/ (image-height TANK) 2)) BACKGROUND))
(check-expect (render-game G2) (render-missiles (game-missiles G2) (render-invaders (game-invaders G2) (render-tank (game-tank G2)))))
;(define (render-game G) BACKGROUND ) ;stub

(define (render-game g)
  (render-invaders (game-invaders g)
                   (render-missiles (game-missiles g)
                                    (render-tank   (game-tank g)))))

;; ListOfInvaders Image -> Image
;; Takes ListOfInvaders and image of missiles and tank , produce new image with the invaders on it's place with the missiles and tank .
(check-expect (render-invaders LOI1 (render-missiles LOM1 (render-tank T1))) (render-missiles LOM1 (render-tank T1)))
(check-expect (render-invaders LOI1 (render-missiles LOM3 (render-tank T1))) (render-missiles LOM3 (render-tank T1)))
(check-expect (render-invaders LOI2  (render-missiles LOM3 (render-tank T1))) (place-image INVADER (invader-x I1) (invader-y I1) (render-missiles LOM3 (render-tank
                                                                                                                                                                        T1
                                                                                                                                                                        ))))
(check-expect (render-invaders LOI3 (render-missiles LOM3 (render-tank T1))) (place-image INVADER (invader-x I2) (invader-y I2) (place-image INVADER
                                                                                                                            (invader-x I1)
                                                                                                                            (invader-y I1)
                                                                                                                            (render-missiles LOM3 (render-tank T1)))))

;(define (render-invaders loin img) img) ;stub

(define (render-invaders loin img)
  (cond [(empty? loin) img]
        [else
         (render-invaders (rest loin) (render-invader (first loin) img))]))


;; Invader Image -> Image
;; Takes Invader and image , adds the invader in the right place without changing anything in the given image 
(check-expect (render-invader I1 (render-missiles LOM3 (render-tank T1)) ) (place-image INVADER (invader-x I1) (invader-y I1) (render-missiles LOM3 (render-tank T1))))
;(define (render-invader invader img) img) ;stub

(define (render-invader invader img)
  (place-image INVADER
               (invader-x invader)
               (invader-y invader)
               img))

;; ListOfMissiles Image -> Image
;; Takes ListOfMissiles and image of the tank , and produce new image with the missiles on it's positions
(check-expect (render-missiles LOM1 (render-tank T0)) (render-tank T0))
(check-expect (render-missiles LOM2 (render-tank T0)) (place-image MISSILE (missile-x M1) (missile-y M1) (render-tank T0)))
(check-expect (render-missiles LOM3 (render-tank T0)) (place-image MISSILE (missile-x M3) (missile-y M3) (place-image MISSILE (missile-x M2) (missile-y M2) (place-image
                                                                                                                                                             MISSILE
                                                                                                                                                             (missile-x M1)
                                                                                                                                                             (missile-y M1)
                                                                                                                                                             (render-tank T0)))))
                                                                                                                            
;(define (render-missiles lom im) img) ;stub

(define (render-missiles lom img)
  (cond [(empty? lom) img]
        [else
         (render-missiles (rest lom) (render-missile (first lom) img))]))


;; Missile Image -> Image
;; Takes missile and image , adds the missile in the right place without changing anything in the given image 
(check-expect (render-missile M1 (render-tank T0) ) (place-image MISSILE (missile-x M1) (missile-y M1) (render-tank T0)))
(check-expect (render-missile M2 (render-tank T0) ) (place-image MISSILE (missile-x M2) (missile-y M2) (render-tank T0)))
;(define (render-missile m img) img) ;stub

(define (render-missile m img)
  (place-image MISSILE
               (missile-x m)
               (missile-y m)
               img))

;; Tank -> Image
;; Takes tank and put it in the right place in the screen
(check-expect (render-tank T0) (place-image TANK (tank-x T0) (- HEIGHT (/ (image-height TANK) 2)) BACKGROUND))
(check-expect (render-tank T1) (place-image TANK (tank-x T1) (- HEIGHT (/ (image-height TANK) 2)) BACKGROUND))

;(define (render-tank t) BACKGROUND) ;stub

(define (render-tank t)
  (place-image TANK (tank-x t ) ( - HEIGHT ( / (image-height TANK) 2)) BACKGROUND))


;; Game -> Game
;; move the tank in click of left or right button , also create missiles when space clicked .
(check-expect (change-tank G0 " ") (make-game empty (cons (make-missile (tank-x (game-tank G0) ) (- HEIGHT TANK-HEIGHT/2)) empty) (game-tank G0)))
(check-expect (change-tank G0 "right") (make-game empty empty (make-tank (tank-x (game-tank G0)) 1)))
(check-expect (change-tank G0 "left") (make-game empty empty (make-tank (tank-x (game-tank G0)) -1)))
(check-expect (change-tank G2 " ") (make-game (game-invaders G2) (cons (make-missile (tank-x (game-tank G2) ) (- HEIGHT TANK-HEIGHT/2)) (game-missiles G2)) (game-tank G2)))
(check-expect (change-tank G2 "right") (make-game (game-invaders G2) (game-missiles G2) (make-tank (tank-x (game-tank G2)) 1)))
(check-expect (change-tank G2 "left") (make-game (game-invaders G2) (game-missiles G2) (make-tank (tank-x (game-tank G2)) -1)))
(check-expect (change-tank G2 "up") (make-game (game-invaders G2) (game-missiles G2) (game-tank G2)))

;(define (move-tank g ke) g) ;stub

(define (change-tank g ke)
  (cond [(key=? ke "left")  (move-left g)]
        [(key=? ke "right") (move-right g)]
        [(key=? ke " ")     (create-missiles g)]
        [else 
              g]))


;; Game -> Game
;; Takes game and change the direction of the movement to the left direction

(check-expect (move-left G0) (make-game empty empty (make-tank (tank-x (game-tank G0)) -1)))
(check-expect (move-left G2) (make-game (list I1) (list M1) (make-tank (tank-x (game-tank G2)) -1)))

;(define (move-left g) g) ;stub

(define (move-left g)
  (make-game (game-invaders g)
             (game-missiles g)
             (make-tank (tank-x (game-tank g) ) -1) ) )

;; Game -> Game
;; Takes game and change the direction of the movement to the right direction

(check-expect (move-right G0) (make-game empty empty (make-tank (tank-x (game-tank G0)) 1)))
(check-expect (move-right G2) (make-game (list I1) (list M1) (make-tank (tank-x (game-tank G2)) 1)))

;(define (move-right g) g) ;stub

(define (move-right g)
  (make-game (game-invaders g)
             (game-missiles g)
             (make-tank (tank-x (game-tank g) ) 1) ) )


;; Game -> Game
;; Takes a game , and create new missiles with x position of the tank .
(check-expect (create-missiles G0) (make-game empty (cons (make-missile (tank-x (game-tank G0)) (- HEIGHT TANK-HEIGHT/2) ) empty) (game-tank G0)))
(check-expect (create-missiles G2) (make-game (game-invaders G2) (cons (make-missile (tank-x (game-tank G2)) (- HEIGHT TANK-HEIGHT/2)) (game-missiles G2)) (game-tank G2)))

;(define (create-missiles G) G) ;stub

(define (create-missiles G)
  (make-game (game-invaders G)
             (cons (make-missile (tank-x (game-tank G)) (- HEIGHT TANK-HEIGHT/2) ) (game-missiles G))
             (game-tank G)))


;; Game -> Bool
;; stops the game when an invader reaches the bottom of the screen
(check-expect (game-over? G0) false)
(check-expect (game-over? G2) false)
(check-expect (game-over? G3) true)

;(define (game-over? g) false) ;stub



(define (game-over? g)
  (cond [(invaders-reached? (game-invaders g)) true]                   
        [else false])) 


;; ListOfInvaders -> Boolean
;; returns true if one of the invaders has reached the bottom of the screen
(check-expect (invaders-reached? empty) false)
(check-expect (invaders-reached? (list I1)) false)
(check-expect (invaders-reached? (list I2)) true)

;(define (invaders-reached? loin) false) ;stub

(define (invaders-reached? loin)
  (cond [(empty? loin) false]                   
        [else (cond [(reached? (first loin)) true] 
                    [else(invaders-reached? (rest loin))])])) 




;; Invader -> Boolean
;; checks whether the invader has reached the bottom of the screen
(check-expect (reached? I1) false)
(check-expect (reached? I2) true)
(check-expect (reached? I3) true)

;(define (reached? invader) false) ;stub


(define (reached? invader)
  (cond [ (>= (invader-y invader) HEIGHT) true]
        [else false]))




