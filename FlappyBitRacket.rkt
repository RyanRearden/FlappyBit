
#lang racket

(require 2htdp/batch-io)
(require 2htdp/image)
(require 2htdp/universe)
(require (only-in racket/gui play-sound))
(require racket/list)
(require racket/file)
(require lang/posn)



;--------------------------------------------------


;Global Variables
(define WIDTH 485)
(define HEIGHT 550)
(define SQRSIZE 40)
(define  HEX (read-words "hex.txt"))
(define BINARY (read-words "binary.txt"))
(define SPEED 7)
(define LASER-LENGTH 10)

;--------IMAGES-----------
(define compsign (bitmap "compscisign-final.png"))
(define computer (flip-horizontal (scale 0.07 (bitmap "coolComputer.png"))))
(define gameOverScreen (scale 0.39 (bitmap "GameOver.png")))
(define SquareHexGif (read-words "GIF\\listing.txt"))
(define GameOverGif (read-words "GameOverGIF\\gameOverListing.txt"))
(define ASCII-chart (scale 1 (bitmap "ASCII-chart.png")))



;----TO GET HEXSQR GIF--------
;Horrible workaround to get the / symbol
;When I tried "//" it would actually prodcuce "//" :(
;Now there is just a text file with the / symbol
(define backslash (first (read-words "backslash.txt")))

;Gets the image read as: "GIF\imagex.gif"
;GIF is the folder that the wings are in
;List refs with a modulo so it counts from 0-howeverlongthelistis and then repeats
(define (GetImage ws)
  (string-append "GIF" backslash (list-ref SquareHexGif (modulo (WS-lineartime ws) (- (length SquareHexGif) 1)))))

;Note: needs bitmap/file to read from a variable
(define (SquareHex ws)
   (freeze (bitmap/file (GetImage ws))))
;-----------------------------

;Gets the image read as: "GameOverGIF\imagex.gif"
;GameOverGIF is the folder that the GameOverImage is in
;List refs with a modulo so it counts from 0-howeverlongthelistis and then repeats
(define (GetGameOverImage ws)
  (string-append "GameOverGIF" backslash (list-ref GameOverGif (modulo (WS-time ws) (- (length GameOverGif) 1)))))

;Note: needs bitmap/file to read from a variable
(define (GameOverIMG ws)
   (scale 0.2 (freeze (bitmap/file (GetGameOverImage ws)))))


;-------------HEX TO BI CONVERSION------------
;;
;;Hex 00-FF are all listed in a file
;;Binary 00000000-11111111 are all listed in a file
;Each position in the hex .txt is the same as the correct binary value in its text file
; That way you can just list-ref the index of the specific file and get the right answer


;String -> String
;Takes a hexidecimal value and gives the binary value
(define (HEXtoBI x)
  (list-ref BINARY (index-of HEX x)))

;String -> String
;Takes a binary value and gives the hexidecimal value
(define (BItoHEX x)
  (list-ref HEX (index-of BINARY x)))

;-------------------------------------------------

;x and y give the location of the object
;Speed is a number - how many pixels the ball moves on each tick
;hex is the hexidecimal value associated with the object
(define-struct HexSqr [x y speed hex])

;BinBlocks are either 0 or 1
;Each corrisponds with a different binaryblock
(define-struct BinBlocks [b1 b2 b3 b4 b5 b6 b7 b8])

; x and y give the location of the laser
; dx and dy are the direction of travel of the laser
; target is what the laser is aiming for
(define-struct Laser [x y dx dy target])

;Binary
;Converts the binary value to hex in the WS
(define-struct Hex->Bin [bin])


; lohs is a list of HexSqr structures
;htb is the conversion of hex to binary
;bbs is a list of BinBlocks sturctures
;lols is a list of laser structures
;time is the time before next HexSqr
;lineartime continously counts up
;score is the success rate of the user
;paused is either true or false -if false then the game continues -if true the game is paused
;loser is either true or false -if false then the game continues -if true the game is over
(define-struct WS [lohs htb bbs lols time lineartime score paused loser])



;List of x positions that fit the hex on screen
;100.8 is the width of the HexSqr with the wings 
;World State -> Number 
(define (random-x ws) (random SQRSIZE (- WIDTH 100.8)))


;Initial conversion in order for nothing to happen
(define initial-hex->bin (make-Hex->Bin "11111111"))

;BinBlocks are the squares at the bottom that the player can manipulate
;These are set to 0 for easy initial use
(define initial-binblocks (make-BinBlocks "0" "0" "0" "0" "0" "0" "0" "0"))

;initial-ws makes sure everything is set up so that the feild is empty 
(define initial-ws (make-WS empty initial-hex->bin initial-binblocks empty 0 0 0 #false #false))


;Random number between 0-255
;Used to get random hex value and same binary counterpart
(define (randomHexVal ws)
  (list-ref HEX (random 256)))

;=====================================================
;=====================================================
;=====================================================


;WorldState, Image -> Image
;Draws the hexSqrs in WorldState ws atop the image img
(define (draw-HexSqr ws nws img)
  (draw-HexSqr-helper ws (WS-lohs ws) img))

;----------------BINARY BLOCKS-----------------------------

;WS -> WS
;Finds the Hex value of the binary blocks
;It converts the binary value of the BinBlocks to the Hex Value
(define (BinaryValue ws)
  (BItoHEX (apply string-append (list (BinBlocks-b1 (WS-bbs ws))
                                      (BinBlocks-b2 (WS-bbs ws))
                                      (BinBlocks-b3 (WS-bbs ws))
                                      (BinBlocks-b4 (WS-bbs ws))
                                      (BinBlocks-b5 (WS-bbs ws))
                                      (BinBlocks-b6 (WS-bbs ws))
                                      (BinBlocks-b7 (WS-bbs ws))
                                      (BinBlocks-b8 (WS-bbs ws))))))

;WS -> WS
;Draws the binary value in nice big text
(define (drawBinaryValue ws)
  (text (BinaryValue ws) 24 "white"))

;WS->WS
;Changes binary values all to 0
;(meant for resetting binary when player converts correctly)
(define (resetBinary ws)
  (make-WS
   (WS-lohs ws)
   (WS-htb ws)
   initial-binblocks
   (WS-time ws)
   (WS-lineartime ws)
   (WS-score ws)
   (WS-paused ws)
   (WS-loser ws)))








;WorldState, Image -> Image
;Draws the score ontop of the playing feild
;The score is how many HexSqrs were solved correctly 
(define (draw-score ws img)
  (above
   (underlay (rectangle WIDTH 50 "solid" (make-color 161 162 234))
                            (text/font (number->string (WS-score ws)) 24 "white"
                                        "Gill Sans" 'swiss 'normal 'bold #f))
                            
   img))



; WorldState, Image -> Image
; Draws binHub from the WorldState ws atop the Image omg
;Creates the 0 0 0 0 0 0 0 0 (or some version with 1s)
;Also adds in the nice looking dirt and grass -wow nice dirt and grass!
(define (draw-binHub ws img)
  (above img
          (place-images
          (list (overlay (text (BinBlocks-b1 (WS-bbs ws)) 40 "White Smoke") (square 50 "solid" "black"))
                (overlay (text (BinBlocks-b2 (WS-bbs ws)) 40 "White Smoke") (square 50 "solid" "black"))
                (overlay (text (BinBlocks-b3 (WS-bbs ws)) 40 "White Smoke") (square 50 "solid" "black"))
                (overlay (text (BinBlocks-b4 (WS-bbs ws)) 40 "White Smoke") (square 50 "solid" "black"))
                (overlay (text (BinBlocks-b5 (WS-bbs ws)) 40 "White Smoke") (square 50 "solid" "black"))
                (overlay (text (BinBlocks-b6 (WS-bbs ws)) 40 "White Smoke") (square 50 "solid" "black"))
                (overlay (text (BinBlocks-b7 (WS-bbs ws)) 40 "White Smoke") (square 50 "solid" "black"))
                (overlay (text (BinBlocks-b8 (WS-bbs ws)) 40 "White Smoke") (square 50 "solid" "black")))
          (list (make-posn 50 50) (make-posn 105 50) (make-posn  160 50) (make-posn 215 50)
                (make-posn 270 50) (make-posn 325 50) (make-posn 380 50) (make-posn 435 50))
           (place-image (BinaryPalace ws) (/ WIDTH 2) 130
          (place-image (bitmap "Dirt.png") (/ WIDTH 2) 100 (rectangle WIDTH 200 "solid" "brown"))))
         ))



;WS->WS
;SHAPE USED FOR SHOWING BIN->HEX CONVERSION (underneath binary blocks)

(define (BinaryPalace ws)
  (place-image (scale 0.1 compsign) 50 50 ;Places sign
               (overlay/offset (overlay/offset (drawBinaryValue ws) 0 10 ;Places hex of binblocks
                        computer) -120 0 ;Draws a copyright-free computer  
                        (rectangle 385 100 "solid" "dimgrey")))) ;Draws the rectangle those things are places on



;Speed is the rate at which the HexSqrs fall in relation to the maximum speed
;The maximum speed is in the tock function -> (ceiling (/ (* SQRSIZE (Speed ws)) SPEED))
(define (Speed ws)
  (if (> (SpeedDeterminer ws) 1)
   (SpeedDeterminer ws)
   1))

;Speed of HexSqrs (every score the value changes and goes to a lower value each time
(define (SpeedDeterminer ws)
  (- 5 (* (WS-score ws) 0.25)))

; List of HexSqrs, Image -> Image
; Draw a list of HexSqrs lohs atop an image img
(define (draw-HexSqr-helper ws lohs img)
  (cond [(empty? lohs) img]
        [else
         (place-image (overlay (text (HexSqr-hex (first lohs)) 24 "black") (SquareHex ws))
                      (HexSqr-x (first lohs))
                      (HexSqr-y (first lohs))
                      (draw-HexSqr-helper ws (rest lohs) img))]))





; List of HexSqrs -> List of HexSqrs
;Takes the HexSqrs and moves them furthur down by speed
(define (move-HexSqrs-helper lohs)
  (cond [(empty? lohs) empty]
        [else (cons (make-HexSqr (HexSqr-x (first lohs))
                                 (+ (HexSqr-y (first lohs)) (HexSqr-speed (first lohs))) ;Changes y position
                                 (HexSqr-speed (first lohs))
                                 (HexSqr-hex (first lohs)))
                    (move-HexSqrs-helper (rest lohs)))]))

; WorldState -> WorldState
;A helper function that makes a new ws with the moved HexSqrs
(define (move-HexSqrs-helped ws)
  (make-WS (move-HexSqrs-helper (WS-lohs ws))
           (WS-htb ws)
           (WS-bbs ws)
           (WS-lols ws)
           (WS-time ws)
           (WS-lineartime ws)
           (WS-score ws)
           (WS-paused ws)
           (WS-loser ws)))

;WorldState -> WorldState
;Moves the hex squares :)
(define (move-HexSqrs ws)
   (shoot-laser (move-HexSqrs-helped (remove-HexSqrs-whenBelow ws))))


;************************Depreciated*************************
;Used to move the squares and remove them from the WS AND CONTINUE THE GAME
;That got really boring because the game just continues on and makes it a lot less stressful
;I kept it in because I kept accidentally creating an infinite loop and spent 2 hours just staring
;at the code until I realized I was referenceing (remove-HexSqrs-whenBelow ws) recursivly
;instead of calling (remove-HexSqrs-whenBelow-helper ws)
;That's what I get for naming things poorly

#|
;WS -> lohs
;Filters out all HexSqrs that are below HEIGHT
(define (remove-HexSqrs-whenBelow-helper ws)
  (filter (lambda (Sqrs) (> (+ HEIGHT (/ SQRSIZE 2)) (HexSqr-y Sqrs))) (WS-lohs ws)))

;WS -> WS
(define (remove-HexSqrs-whenBelow ws)
    (make-WS
     (remove-HexSqrs-whenBelow-helper ws)
     (WS-htb ws)
     (WS-bbs ws)
     (WS-lols ws)
     (WS-time ws)
     (WS-lineartime ws)
     (WS-score ws)
     (WS-paused ws)
     #true))
|#

;*******************************************


;WS -> boolean
;Filters out all HexSqrs that are not below HEIGHT
(define (belowGround? ws)
  (not (empty? (filter (lambda (Sqrs) (< (+ HEIGHT (/ SQRSIZE 2)) (HexSqr-y Sqrs))) (WS-lohs ws)))))

;WS -> WS
;Stops the game by making (WS-loser ws) true 
(define (remove-HexSqrs-whenBelow ws)
  (if (belowGround? ws)
    (make-WS
     (WS-lohs ws)
     (WS-htb ws)
     (WS-bbs ws)
     (WS-lols ws)
     (WS-time ws)
     (WS-lineartime ws)
     (WS-score ws)
     (WS-paused ws)
     #true) ;right here
    ws))




;WorldState ->Number
;Determines the HexSqr that was solved
;With Binary
;make a list of binary value = one of the squares (there should just be 1 thing in the list) 
(define (find-solved-HexSqr ws)
  (filter (lambda (Sqrs)  (string=? (BinaryValue ws) (HexSqr-hex Sqrs))) (WS-lohs ws)))


;-----LASERS----------------------------------------------------------


; Helper function to draw one laser on an image
; Laser, Image -> Image
(define (draw-one-laser laser img)
  (place-image
   (rectangle  5 LASER-LENGTH  "solid" "red")
   (Laser-x laser)
   (Laser-y laser)
   img))

; List of Lasers, Image -> Image
; Draws each laser, helper function
(define (draw-lasers-helper lols img)
  (foldl draw-one-laser img lols))

; Laser -> Laser
; Move a laser according to its dx and dy
(define (move-one-laser laser)
  (make-Laser (+ (Laser-dx laser) (Laser-x laser))
              (+ (Laser-dy laser) (Laser-y laser))
              (Laser-dx laser) ( - (Laser-dy laser) 2) ;Makes acceleration possible--solves a bug (mostly) 
              (Laser-target laser)))                   ;Drives the speed of the laser 


; WorldState -> WorldState
; deals with laser movement
(define (move-lasers ws)
  (laser-hit
   (move-all-lasers ws)))

; list of lasers -> list of lasers
; Moves each laser in lols
(define (move-all-lasers-help lols)
  (map move-one-laser lols))

; WorldState -> WorldState
;Moves all lasers by making a new world state 
(define (move-all-lasers ws)
  (local [(define olol (WS-lols ws))
          (define new-lols (move-all-lasers-help olol))]
    (make-WS
     (WS-lohs ws)
     (WS-htb ws)
     (WS-bbs ws)
     new-lols
     (WS-time ws)
     (WS-lineartime ws)
     (WS-score ws)
     (WS-paused ws)
     (WS-loser ws))))



;WS -> Boolean
;Checks if a HexSqr has been solved or not
;AND if there is a laser already approaching the hex square that has been solved
;Solved the bug of launching 2 lasers at 1 HexSqr and the game getting confused
(define (check-solved ws)
  (if (and
       (not (empty?(find-solved-HexSqr ws)))
       (not (member (HexSqr-hex (first (filter (lambda (Sqrs)  (string=? (BinaryValue ws) (HexSqr-hex Sqrs))) (WS-lohs ws))))
                    (map (lambda (x) (Laser-target x)) (WS-lols ws)))))
       #true #false))



;WorldState -> WorldState
;Shoots the laser at the x value that the target is at
(define (shoot-laser ws)
  (if (check-solved ws)
      (make-WS
       (WS-lohs ws)
       (WS-htb ws)
       initial-binblocks
       (cons (make-Laser (HexSqr-x (first (find-solved-HexSqr ws))) HEIGHT 0 -5
                         (HexSqr-hex (first (find-solved-HexSqr ws)))) (WS-lols ws))
       (WS-time ws)
       (WS-lineartime ws)
       (WS-score ws)
       (WS-paused ws)
       (WS-loser ws))
      ws))


;WorldState -> WorldState
;Checks if the y value for the HexSqr target and the laser targeter are the same
;(same as in Laser-y is greater than HexSqr-y)
(define (laser-hit ws)
  (cond [(empty? (WS-lols ws)) ws]
        [(< (abs (- (Laser-y (last (WS-lols ws))) (/ SQRSIZE 2)))
            (HexSqr-y (matched-HexSqr ws)))
         (delete-lasers (remove-HexSqrs ws))]
        [else ws]))

;WS -> WS
;Filters out all of the HexSqrs that do not have the last laser being shot at it
;That means it is only checking one laser with one hex sqr
(define (matched-HexSqr ws)
  (first (filter (lambda (Sqrs)
                   (string=? (Laser-target (last (WS-lols ws)))
                             (HexSqr-hex Sqrs)))
                 (WS-lohs ws))))


; WS -> WS
; Produce a new list of HexSqrs with only those HexSqrs in lohs
; that have not been solved
(define (remove-HexSqrs ws)
  (local [(define WSLohs ws)
          (define new-lohs (remove-HexSqrs-helpering ws))]
    (make-WS
     new-lohs
     (WS-htb ws)
     (WS-bbs ws)
     (WS-lols ws)
     (WS-time ws)
     (WS-lineartime ws)
     (WS-score ws)
     (WS-paused ws)
     (WS-loser ws))))


;WS -> WS
;Filters out the HexSqrs that have the same Hex Value
;Solves the problem of the lasers not getting confused
;Also checks the height but that isn't really needed
(define (remove-HexSqrs-helpering ws)
  (filter (lambda (Sqrs) (and (> HEIGHT (HexSqr-y Sqrs))
                          (not (string=? (Laser-target (last (WS-lols ws))) (HexSqr-hex Sqrs))))) (WS-lohs ws)))


;lols -> lols
;Removes the last laser item
(define (delete-lasers-helper lols)
  (reverse (rest (reverse lols))))

;WS -> WS
;Makes a new list of lasers without the last item
;Also adds 1 to the score
;Also resets time 
(define (delete-lasers ws)
  (local [(define olol (WS-lols ws))
          (define new-lols (delete-lasers-helper olol))]
    (make-WS
     (WS-lohs ws)
     (WS-htb ws)
     (WS-bbs ws)
     new-lols
     (- (WS-time ws) (WS-time ws))
     (WS-lineartime ws)
     (+ (WS-score ws) 1)
     (WS-paused ws)
     (WS-loser ws))))




;Running into the issue of if the second laser hits before the first one
;Then it passes straight through
;Trying to make it so that the code always checks the closest instead of the earliest
;UPDATE The laser acceleration helped and the problem is now mostly solved :)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;------------------------------------------------------------------------------------------------------------

; WorldState -> WorldState
; Sometimes, randomly, add a random HexSqr to the WS
(define (add-HexSqrs-maybe ws)
  (make-WS
   (cons (get-compatible-hexsqr (WS-lohs ws)) (WS-lohs ws))
   (WS-htb ws)
   (WS-bbs ws)
   (WS-lols ws)
   (WS-time ws)
   (WS-lineartime ws)
   (WS-score ws)
   (WS-paused ws)
   (WS-loser ws)))



;FOR COMPATABILITY
;Makes sure that there are not 2 HexSqrs with the
;same hexidecimal value
;Since there cannot be 255 HexSqrs on the board this should
;always work 
(define (get-compatible-hexsqr lohs)
  (local [(define newsqr (make-HexSqr (random SQRSIZE (- WIDTH SQRSIZE))
                                      0 SPEED (list-ref HEX (random 1 256))))]
    (if (is-compatible? newsqr lohs) newsqr (get-compatible-hexsqr lohs))))  

;hxsqr&lohs -> list
;Makes a list of hexSqrs that do not have the same value
(define (is-compatible? hxsqr lohs)
  (foldl (lambda (li acc) (and (notsame li hxsqr) acc)) #true lohs)) 

;Checks the value of two hexsqrs
(define (notsame hxsqr1 hxsqr2)
  (not (string=? (HexSqr-hex hxsqr1) (HexSqr-hex hxsqr2))))
;---------------------------------------------------------


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;---------------TOCK-------------------------
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;Tock moves at 28 fps
;If the game hasn't ended continue Else produce a world state
;Adds 1 HexSqr when the time is right
;Moves everything without adding a HexSqr when the time isn't right
;HexSqr release is guided by X/V = t
(define (tock ws)
  (cond [(WS-loser ws)
         (make-WS
          empty
          (WS-htb ws)
          (WS-bbs ws)
          empty
          (+ (WS-time ws) 1)
          0
          (WS-score ws)
          (WS-paused ws)
          (WS-loser ws))]
        [(WS-paused ws)
         ws]
        [else (if (= (WS-time ws) (ceiling (/ (* SQRSIZE (Speed ws)) SPEED)))
      (subtractALL (move-lasers  (add-HexSqrs-maybe (move-HexSqrs ws))))
      (addONE (move-lasers (move-HexSqrs ws))))]))
;note: ceiling always rounds up


;WS -> WS
;Adds 1 to time to help decide when to make a new HexSqr
;Adds1 to lineartime
(define (addONE ws)
  (make-WS
   (WS-lohs ws)
   (WS-htb ws)
   (WS-bbs ws)
   (WS-lols ws)
   (+ (WS-time ws) 1)
   (+ (WS-lineartime ws) 1)
   (WS-score ws)
   (WS-paused ws)
   (WS-loser ws)))
;WS -> WS
;Makes time 0 to restart clock
;Continues to add1 to linear time
(define (subtractALL ws)
  (make-WS
   (WS-lohs ws)
   (WS-htb ws)
   (WS-bbs ws)
   (WS-lols ws)
   (- (WS-time ws) (WS-time ws))
   (+ (WS-lineartime ws) 1)
   (WS-score ws)
   (WS-paused ws)
   (WS-loser ws)))
  
  

;---------------------------------------------------


;String -> String
;Takes in "bit" and converts it
;to the value it isn't
(define (changeNum binblk)
  (if (string=? binblk "0") "1" "0"))






;Key Handler
;Key Handler takes a letter and changes the value of the
;respective binary block 
;World State -> World State
(define (key-handler ws key)
  (cond [(string=? key "a")
         (make-WS
          (WS-lohs ws)
          (WS-htb ws)
          (make-BinBlocks (changeNum (BinBlocks-b1 (WS-bbs ws)))
                          (BinBlocks-b2 (WS-bbs ws))
                          (BinBlocks-b3 (WS-bbs ws))
                          (BinBlocks-b4 (WS-bbs ws))
                          (BinBlocks-b5 (WS-bbs ws))
                          (BinBlocks-b6 (WS-bbs ws))
                          (BinBlocks-b7 (WS-bbs ws))
                          (BinBlocks-b8 (WS-bbs ws)))
          (WS-lols ws)
          (WS-time ws)
          (WS-lineartime ws)
          (WS-score ws)
          (WS-paused ws)
          (WS-loser ws))]
        [(string=? key "s")
         (make-WS
          (WS-lohs ws)
          (WS-htb ws)
          (make-BinBlocks (BinBlocks-b1 (WS-bbs ws))
                          (changeNum (BinBlocks-b2 (WS-bbs ws)))
                          (BinBlocks-b3 (WS-bbs ws))
                          (BinBlocks-b4 (WS-bbs ws))
                          (BinBlocks-b5 (WS-bbs ws))
                          (BinBlocks-b6 (WS-bbs ws))
                          (BinBlocks-b7 (WS-bbs ws))
                          (BinBlocks-b8 (WS-bbs ws)))
          (WS-lols ws)
          (WS-time ws)
          (WS-lineartime ws)
          (WS-score ws)
          (WS-paused ws)
          (WS-loser ws))]
        [(string=? key "d")
         (make-WS
          (WS-lohs ws)
          (WS-htb ws)
          (make-BinBlocks (BinBlocks-b1 (WS-bbs ws))
                          (BinBlocks-b2 (WS-bbs ws))
                          (changeNum (BinBlocks-b3 (WS-bbs ws)))
                          (BinBlocks-b4 (WS-bbs ws))
                          (BinBlocks-b5 (WS-bbs ws))
                          (BinBlocks-b6 (WS-bbs ws))
                          (BinBlocks-b7 (WS-bbs ws))
                          (BinBlocks-b8 (WS-bbs ws)))
          (WS-lols ws)
          (WS-time ws)
          (WS-lineartime ws)
          (WS-score ws)
          (WS-paused ws)
          (WS-loser ws))]
        [(string=? key "f")
         (make-WS
          (WS-lohs ws)
          (WS-htb ws)
          (make-BinBlocks (BinBlocks-b1 (WS-bbs ws))
                          (BinBlocks-b2 (WS-bbs ws))
                          (BinBlocks-b3 (WS-bbs ws))
                          (changeNum (BinBlocks-b4 (WS-bbs ws)))
                          (BinBlocks-b5 (WS-bbs ws))
                          (BinBlocks-b6 (WS-bbs ws))
                          (BinBlocks-b7 (WS-bbs ws))
                          (BinBlocks-b8 (WS-bbs ws)))
          (WS-lols ws)
          (WS-time ws)
          (WS-lineartime ws)
          (WS-score ws)
          (WS-paused ws)
          (WS-loser ws))]
        [(string=? key "g")
         (make-WS
          (WS-lohs ws)
          (WS-htb ws)
          (make-BinBlocks (BinBlocks-b1 (WS-bbs ws))
                          (BinBlocks-b2 (WS-bbs ws))
                          (BinBlocks-b3 (WS-bbs ws))
                          (BinBlocks-b4 (WS-bbs ws))
                          (changeNum (BinBlocks-b5 (WS-bbs ws)))
                          (BinBlocks-b6 (WS-bbs ws))
                          (BinBlocks-b7 (WS-bbs ws))
                          (BinBlocks-b8 (WS-bbs ws)))
          (WS-lols ws)
          (WS-time ws)
          (WS-lineartime ws)
          (WS-score ws)
          (WS-paused ws)
          (WS-loser ws))]
        [(string=? key "h")
         (make-WS
          (WS-lohs ws)
          (WS-htb ws)
          (make-BinBlocks (BinBlocks-b1 (WS-bbs ws))
                          (BinBlocks-b2 (WS-bbs ws))
                          (BinBlocks-b3 (WS-bbs ws))
                          (BinBlocks-b4 (WS-bbs ws))
                          (BinBlocks-b5 (WS-bbs ws))
                          (changeNum (BinBlocks-b6 (WS-bbs ws)))
                          (BinBlocks-b7 (WS-bbs ws))
                          (BinBlocks-b8 (WS-bbs ws)))
          (WS-lols ws)
          (WS-time ws)
          (WS-lineartime ws)
          (WS-score ws)
          (WS-paused ws)
          (WS-loser ws))]
        [(string=? key "j")
         (make-WS
          (WS-lohs ws)
          (WS-htb ws)
          (make-BinBlocks (BinBlocks-b1 (WS-bbs ws))
                          (BinBlocks-b2 (WS-bbs ws))
                          (BinBlocks-b3 (WS-bbs ws))
                          (BinBlocks-b4 (WS-bbs ws))
                          (BinBlocks-b5 (WS-bbs ws))
                          (BinBlocks-b6 (WS-bbs ws))
                          (changeNum (BinBlocks-b7 (WS-bbs ws)))
                          (BinBlocks-b8 (WS-bbs ws)))
          (WS-lols ws)
          (WS-time ws)
          (WS-lineartime ws)
          (WS-score ws)
          (WS-paused ws)
          (WS-loser ws))]
        [(string=? key "k")
         (make-WS
          (WS-lohs ws)
          (WS-htb ws)
          (make-BinBlocks (BinBlocks-b1 (WS-bbs ws))
                          (BinBlocks-b2 (WS-bbs ws))
                          (BinBlocks-b3 (WS-bbs ws))
                          (BinBlocks-b4 (WS-bbs ws))
                          (BinBlocks-b5 (WS-bbs ws))
                          (BinBlocks-b6 (WS-bbs ws))
                          (BinBlocks-b7 (WS-bbs ws))
                          (changeNum (BinBlocks-b8 (WS-bbs ws))))
          (WS-lols ws)
          (WS-time ws)
          (WS-lineartime ws)
          (WS-score ws)
          (WS-paused ws)
          (WS-loser ws))]
        [(string=? key "p")
          (make-WS (WS-lohs ws)   
           (WS-htb ws)            
           (WS-bbs ws)
           (WS-lols ws)
           (WS-time ws)
           (WS-lineartime ws)
           (WS-score ws)          ;If "p" is pressed 
           (not (WS-paused ws))   ;Either pause or unpause the game
           (WS-loser ws))]
        [(string=? key "\r")
         (if (WS-loser ws)  ;If the game is over 
         initial-ws         ;Then press enter to start the game again
         ws)]
        [else ws]
        )) 



;--------RENDER-------------------------------------
;WorldState -> Image
;If the game is over-display gameOVer
;If the game is paused-display gamePaused
;Else draw the normal image
(define (render ws)
  (cond [(WS-loser ws)
         (gameOver ws)]
        [(WS-paused ws)
          (gamePaused ws)]
        [else
         (draw-score ws (draw-binHub ws(draw-lasers-helper (WS-lols ws) (draw-HexSqr ws ws
                                                                     (place-image  (bitmap "Sky.png")
                                                                                         (/ WIDTH 2) (/ HEIGHT 2)
                                                                                   (rectangle WIDTH HEIGHT "solid" "lightblue"))))))]))

;WS -> img
;gameOver is the result of missing a HexSqr. It promts you to retry-the game
;In the key-handler, if the "enter" key is triggered ("\r") then the game
;is allowed to restart
(define (gameOver ws)
  (overlay/offset (overlay/offset
                   (GameOverIMG ws) 
                   0 200
                                  (text "Press ENTER to play again" 35 "black")) 0 100
                  (overlay gameOverScreen (rectangle WIDTH 790 "solid" "red"))))


;WS -> img
;gamePaused is the result of pressing "p"
;It pauses the game and displays an ASCII chart (with some friends)
;When the user presses "p" again, it unpauses the game and allows the user
;to continue from where they left off
;SOUND WARNING 
(define (gamePaused ws)
  (cond [(WS-paused ws)
       (begin (play-sound (build-path (current-directory) "ChristmasSong.wav") #t)
         (overlay ASCII-chart (rectangle WIDTH 790 "solid" "white")))]
        [else ws]))


  
  
;------------------------------------------------



;World State -> World State
;To-draw renders the ws
;On Key is uses to use keys
;On tick make time move
(big-bang initial-ws
  (to-draw render)
  (on-key key-handler)
  (on-tick tock))





;@@@@@@@@@@@@@@@@@@@@@@@@&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&%%&%%%%%%%%&&&&&&&&&&&%%%&&&&&%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&%%%%%%%%&&&%%%&&&&&%%&&&&&&%%&&%&&&&&&&&&&&&&&&&&%%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&@&@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&%%%%%%%%%%%%%%%%%%%%&%&&&&&&&%&&&&%%&%&&&&&%%&&&&&&&%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&%%%%##%&&&#%%&&&%&&%%%%%%%%%%%%##%%&%%##%%%&%%%%&@&&&&&@&&&&%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&%###(#%%%#%%%%%%%&&&%%%%%%%&&%%%%%%%%%%%%##%%&&%%##%&&%%&&@&&&&&%%%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&%%%((##%#&%####%%%%&&&%%%&%%%%%%%%######%%%########%@@&#%%%%&&&&&%%&&&%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&%(((##%%%%####%%%%%%%&%%%%%%%%%%%%%###%%####%%%%%%&%%%#%%&&%%&%%&&&&&&&&&&%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&#/(#(/#%%((%%%%&&%#%%%##%%#%%##((((////////////((#######%%&%%&@&&&&&@@&@&&%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&%///(((%#/###%%%############(((/////////************////((((##%%%%&%%&@&&@@@@&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&%#/*(##%#(/#%&&%((#%#(#%##((////////**************************//((((%%%%&&&&@@@@@@&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&#*////(/#%%&&&%####((##(((//////***************,*,,,,,************///(#%&%&&&@@@@@@@&%%%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&#/*//((#%%%&&&&%###((((((((/////*************,,,,,,,,,,,,,,***********////(%&@@@@&@@@@@&%%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&@@@@@@@@@
;@@@@@@@@@@@@@&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&%((#(###%&&&&%%%####((((((((////************,,,,,,*,,,,,,,,,,,,**********//((#%&&@&&@&@@&%%%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&@@@@@@@@@
;@@@@@@@@@@@@&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&%((((##%%%&%&%%#####((((((///////*************,,,,,,,,,,,,,,,,,,,,,,*******////#%&&@&&&&&&%%%%%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&@@@@@@@@
;@@@@@@@@@@@@&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&%%%((##((%%&&%###(((((((((/////***********,,,,,,,,,,.......,,,,,,,,,,,*******//(##&&@@&%%&%%%%%%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&@@@@@@@@@
;@@@@@@@@@@@@&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&%%%((%%##%%##%#(((//(((//////************,,,,,,,,,,.,,..,,,.,,,,,,,,,,*******////#%%&&%#%&%%%%%%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&@@@@@@@@
;@@@@@@@@@@@&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&%((%%#((##%###(//((((///////**********,,,,,,,,,,,,,.,,.....,,,,,,,,,,,,*****/////##%%%%%%&%%%%%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&@@@@@@@@@
;@@@@@@@@@@@&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&%%&##%#(((%#(#%#((((((((///////**********,,,,,,,,,,,,,,...,,,,,,,,,,,,,,,,,,****////(%####%%%&%%%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&@@@@@@@@@
;@@@@@@@@@@@&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&%((###%%(######(((((((//////*************,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,******//(#%%%%%&&%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&@@@@@@@@@
;@@@@@@@@@@&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&%###%#/#&%(####(((((((//////**********,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,*****///#%%&&&%%&%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&@@@@@@@@
;@@@@@@@@@@&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&%%((%%%&%(#######(((/////////**********,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,*******//(##%&&&&%%%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&@@@@@@@@
;@@@@@@@@@@&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&%/#&%%(#&########(**///(((////******,*,,,,,,,,,,,,,,,,,,,,,.,,.,,,,,,,,,*********//(%%&&%%%&%%&&%%&&%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&@@@@@
;@@@@@@@@@&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&%,./#/(*/#&&#(####(//((((########%%#####((///////*//////////((((#####(///////****////(%&&&&%%&%%&%%%%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&@@@@@@
;@@@@@@@@@&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&%(##(**/*,(#(#%#(((/(%%%%#(/////***///(((((/***/*********/#%(///(((####((((//*****/**(#&&%&%%%%%%%%%%&%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&@@@@
;@@@@&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&%(#/*(////((#%###(#%#####((((#%%%%%%%#((((((#(((#####(((((*//////////******///////((#(**%&((((%%&%%%%&&&%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&@&&&@@@
;@@@&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&%((#((///((((/*#@@@%(##%##%%&%#%%#@#*#*(%((####(%((/*,,*/((,*///(#%%&%&@%%%#////(((((((#&%#((#(#&&&%%%%%%%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&@@
;@@@&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&%&%((((#((#(((****,(#/((((###((((#####**//(##(/(%&#(***,,*%%/,,**///#(%#/%/*#%%#(///////##%##(*/%%%%%%%%%%%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&@@@
;@@&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&%(((#####(((/*/*,(#*///((((((/////*****///(((#&#((******,(&*,,**/*/(((/*,***/((///***(#/##//*(&%%%%%%%%%%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&@
;@@&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&%(/(%#(#((((//*,,.((,*//((////**********//(//%%#(//*******(#,,******,,,,,,,****//***/#(/((**/%&%%%%%%%%%%%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
;@@&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&%%%%%&#((#%#(**(/**,,,,,#/,****///***********//*(&#((((//********#(.,*,,,.....,,,******,(#*//(/*,*%&%%%%%%%%%%%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
;@@&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&%%&%%%%&%((((#(**/(*,,/(////((//****,,,,,,,**//(#%#(/((((//*******,*/((/***,,,,,,,*////((/**//(//*,,#&%%%%%%%%%%%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
;@@&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&%%%%%%%%%&#/**((,*///**(((((((//*/(#%%%%%%%##(/*///(((((((/***,,******,,*////((///**,,,,,,***/((//*,,#&%%%%%%%%%&%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
;@&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&%%&%%%%%&%(//(***//*/(((((///**********,******/((((((((((**,,,********,,,,,,,,,,,,,,,,,****/((/****%&%%%%%%%%%%&&%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
;&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&%&&%%%%%%&%#((/,**/*/((((((//******,********/(#((((//(((/**,,,,**********,,,.,,,,,,,,,,,,**/((/,**#&%%%%%%%%%%%%%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
;&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&%%%%%%%%%&&%%%%%%%%%%#///*,,*/(###(((///***********/(#%###((((((/***,,,.,,******///**,,..,,,,,,,****/((**,#&%%%%%%%%%%%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
;&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&%%%%%%%%%%%%%%%%%%%%%%&%(*,,,*((#####((///*******//(((((###%%&&%%#((*****//(#(/***/((//**,,,,,,,,****/(/,/&&%%%%%%%%%%%%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
;&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&%%%%%%%%%%%%%%%%%%%%%%%&%/*,,/(#######((///////(#(((((##((#%%###%%%####(//(((((/////(%(/****,,*******/(/#&%%%%%%%%%%%%%%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
;&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&%%%%%%%%%%%%%%%%%%%%%%%%%#/.,,/(#######((//(/#%#(####(###%%%##%%%#####%##(###(#(/(#(((/#&#**********////#&%%%%%%%%%%%%%%%%%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
;&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&%%%%%%%%%%%%%%%%%%%%%%%%%&/,,,/###((##((//*/##%%#%%#%##%##%#######(((###((((#(((##(####((%#/*******////*(&%%%%%%%%%%%%%%%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
;&&&&&&&&&&&&&&&&&&&&&&&&&&&&%%&&%%%%%%%%%%%%%%%%%%%%%%%%%#*,,/((((((##///**(##(((%@@%%%%######(((/(/((/(//(////((###%%####/********//**#%%%%%%%%%%%%%%%%%%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
;&&&&&&&&&&&&&&&&&&&&&&&&&&&&&%%%%%%%%%%%%%%%%%%%%%%%%%%%%&#,**/(#((((((//**/#%(/////(##((*,/,.**,,*/,..*,.**,*,,#&#((###(/*********//*/#%%%%%%%%%%%%&%%%%%%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
;&&&&&&&&&&&&&&&&&&&&&&&&&&&&&%&&%%%%%%%%%%%%%%%%%%%%%%%%%%#/*//(((((//((/**/###(/(/((((/////*..,.....  ...,,,*//*****(#(******/****///(&&%%%%%%%%&&&&&&%%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
;&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&%%%%%%%%%%%%%%%%%%%%%%%%%%%/*/(#((///((//(#%#((//(((((((/******,,**,,,,,,,*********/#(/*********/***#&%%%%%%%%%%&&&%%%%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
;&&&&&&&&&&&&&&&&&&&&&&&&&&&&%%%&%%%%%%%%%%%%%%%%%%%%%%%%%%@( */(#/(////((#%%#(((/(/((((((///*****,,,,,,,,,*********//##(**//********#&%%%%%%%%%%%%%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
;&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&%%%%%%%%%%%%%%%%%%%%%%%%%&@&. *,*/((/(((###&%(((/(#((((##((//////****************/***//#((/*******/**#&%%%%%%%%%%%%&&%%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
;&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&%%%%%%%%%%%%%%%%%%%%%%&@@@. //,**//(#%#((%%%#%#(#((////(((((/((((/((//////*//////**//(((//********/%%%%%%%%%%%%%%%%%%%%&%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
;&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&%%%%%%%%%%%%%%%%%%%%%&@@@, ,(/,*/(/(####%%##%#(#(/*////////(((/(((((/(//(/************/((///*//**/%&%%%%%%%%%%%%%%%%%%%&&&%%&&&%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
;&&&&&&&&&&&&&&&&&&&&&&&&&&&&&%%%%%%%%%%%%%%%%%%%%%%&@@@/ .*#*,*/###((%%&%####(//((///*/(////(**//*/****,**,,*****,**/*/(//////*#&&%%%%%%%%%%%%%%%%%%%%%%%%&&&%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
;&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&%%%%%%%%%%%%%%%%%%%%&@@@&. ,(%*,/(#######%&&&&##((/////**//(/////*******//*****,*,,*****/#(///(#%&&%%%%%%%%%%%%%%%%%%%%%%%%%&%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
;&&&&&&&&&&&&&&&&&&&&&&&&&&&&%%%%%%%%%%%%%%%%%%%&%&&@@@#..,/%/*/(##########%%##(/****/*//*/(////***********,*,*,,,***/(##(##%%&%&&&%%%%%%%%%%%%%%%%%%%%%%%%%%%%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
;&&&&&&&&&&&&&&&&&&&&&&&&&&&&&%%%%%%%%%%%%%%%&&&%%&&@@@#*,.*##*/(############%###(/////*/(/*//*//////**,**,,,,,,,***/(((,,#&%%%&&%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
;&&&&&&&&&&&&&&&&&&&&&&&&&&&%%%%%%%%%%%%%%%%&&&%%&&@@@@&/,.,/%#((####((###########((////(////*/*(((/**/*,,**,**,**///(/(*. /&%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
;&&&&&&&&&&&&&&&&&&&&&&&&&&%%%%%%%%%%%%&&%&&&&&&&&&&@@@@#*,,*/#((((##(##############((((/*//((((*///**/*****,,**///////((,. *%&%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
;&&&&&&&&&&&&&&&&&&&&&&&%&&&%%%%%%%%&&@&&&&&&&&&&&&&@@@@&/*,,,/%(((((((((((((#(##(####(//#(((((##//(#/*/**/**/**/**///(((/,  *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
;&&&&&&&&&&&&&&&&&&&&&&&%%%%%%%%%&&@&&&&&&&@@@&&&&&@@@@@@&/**,,*##(((((((((((((((((((#########((#(((////////////**//*(%(///.. *&&&%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
;&&&&&&&&&&&&&&&&&&&&%%%%%%&&&&&&@&&&@&&@@@@@@@&&&&@@@@@@@%/*,,,,*%(/(((((((((((((((((((((((((((((////**/*****///**//(&(///*.  (@&@@@@@&&%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
;&&&&&&&&&&&&&&&&%%%%%&&&&&&&@@@@@&@@@&&@@@@@@@@@&&&@@@@@@@%*/*,,.,##/////(///(((((((((((((((/////**********////*/////%(/*/*....%&&&@@@@@@@@@&&&&&&%%%%%%%%%%%%%%%%%%%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
;&&&&&&&&&&&&%%%%&&&&&&@@@@@@@@@@@@@@@&&@@@@@@@@@&@@@@@@@@@@#*/*,,..*#(//////////(((((/((((//////**********//****///*/%#*****.  (&%&&&@@@@@@@@@@@@@@@@@&&&%%%%%%%%%%%%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
;&&&&&&&&&&&&&&&&&@@@@@@@@@@@@@@@@@@@@@&@@@@@@@@@&&@@&@@@@@@@#**/*,...*((//////////////////((///*****************//**/&#,*/***. .&&%&&&&@@@@@@@@@@@@@@@@@@@@@@&&%%%%%%%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
;&&&&&&&&&&&@@@@@@@@&@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@&&@@@&@@(*****....*((//*//**///////***///(//////**********//**/(&(*/****.  #@&%&&&&&@@@@@@@@@@@@@@@@@@@@@@@@&&%%%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
;&&&&&&@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@&@@@@&@@&&@&&&&&&&&@@&@&(*****,,.  *(///***********///***//***************/**/(&#****,,,. *@&&&&&&&&&@@@@@@@@@@@@@@@@@@@@@@@@@@@&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@&&&&@&@&&&&&&&&&&&@&#*******,.  ,//*********//**********************/***/#&#***,,,,. .%@&&&&&&&&&&@@@@@@@@@@@@@@@@@@@@@@@@@@@@@&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@&@@@@@@&&&@&@&&&&&&&@@@&&&@#*,*,,,,,,.  .,***********************************/(%&(**,,..,.  #@&&&&&&&&&&&&@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@&&&&&&&&&&&&&&&&&&&&&&&&&&
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@&@@@@@@@@&&@@&&&&&&&&&&&&&&@#*,,,,,,,,,,.   *******/*************************/#&%/,,,,..... /@&&&&&&&&&&&&&&@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@&&&&&&&&&&&&&&&&&&&&&
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@&&&&&&&&&&&&&&&&&&&&@#/*,,,,,,,,,,,,  .*****************************/(%&#*,,....... /@&&@@@&&&&&&&&&@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@&&&&&&&&&&&&&&&&&&&
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@&&@@@@&&@&&&&&&&@@&&&&@#//,,,,,...,,,**.  ,*****************,,,*****/(#%%(,,..,....  ,&@@@@@@@@&&&&&&@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@&&&&&&&&&&&&&&&&&
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@&&&@&@&&&&&&@@@@@@@&&&@%//,,,,...,,,,**,,. .***********,,,,,,,*****/(#%%*...,,,....  #@&&@@@@@@@@&&&@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@&&&&&&&&&&&&&&&&
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@&@@@@@&&&&@@@@@@@@&@@&&@%//*,,,...,,,,,**,..  ,*,**,,,,,,,,,,,,,**//(#%(,..,,,... ,, *@@&@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@&&&&&&&&&&&&&&&&
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@&&@%,**,,,,,,.,,,,,,,... .,,,,,,,,,,,,,,,,**/(##%*.,..,....,,.*%@&@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@&&&&&&&&&&&&&&&
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@&@&@@@@@@@@@@@@@@@@@@@@@@@&&@%..,,,,..,,,,,,,,,,,.   ,,,,,,,,,,,,,,**/(#%*.,,,,. ,..*&@@&&&&@&@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@&&&&&&&&&&&&@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@&&&@%.....,....,,,,,,,,,.,*. ,,,,,,,,,,***/(##,,,.. .,...,%@@&&&&&&&@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@&&&&&&&&&&@&@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@&&&&@&.........,,,,,,,,,****, ,*,*******///##*,. .,,......(@@&&&@&@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@&&&&&&&&&&&&&@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@&&&&@&...............,*,,,,,,..******//((/*,.. .,.  .  ..*&@@&&&&&@@@@@@@@&&@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@&&&&&&&&@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@&&&&@&. ...,,. .. .*,,,.,,.,,.,***/(#* ....  ...      ..,#@@@&&@@&@@@@@@@@@@&&&&@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@&&&&&&&&&&@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@&&&&&@&. ....... .,**....,,,,..*((#* ...              .../&@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@&&&&&&&&&&@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@&@&&&&&@&.  ..   .  .,**.....,.,#%#. .                  ..,%@@@&&@&@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@&&&&&&&&&@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@&&&&&@&, .     .  .,**,,..   ./,                     ...(@@@@&@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@&&&&&&&&&&@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@&&&&@&, .        ..,,,,,,..,,.                     ...*&@@&&@&&@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@&&&&&&&&&&@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@&&&@@* ...       ...   ..,.                      ....%@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@&&&&&&&&&@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@&&&@@, ...            .,,.                       ...(@@@&&&&@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@&&&&&&&@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@&&@@, ....          .,,.                       ...*&@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@&&&&&@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@&@@&&&@@, .....        ..,.                       ....#@@&&@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@&&&&@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@&&&@@, ......      .,,.                       ..../&@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@&&&&&@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@&&&@@, ......      .,,                       ....,%@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@&&&@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@&&@@@, ......      ,,                        ....#&@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@&&&@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@&@@@, .......    .*.   ....                ..../&&@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@&&&@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@, .......    ,,   ....         .      ....,%&@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@&&&&@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@&&@@@, .......   .,                ..     .....(&@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@&&@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@&&@@* ........  ,.               ..      ..../%&@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@&@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@&@@@/ .......  ,,               ..      ....,#&@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@&@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@* ....... .,               ......  .....(&@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@&@@@@@@@@


;Art generated by https://manytools.org/hacker-tools/convert-images-to-ascii-art/go/

