;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |HtDW Design Quiz|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Design a World Program with Compound Data.
;; Create a program that allows you to click on a spot on the screen to create a flower, which then grows over time.
;; If you click again the first flower is replaced by a new one at the new position.

(require 2htdp/image)
(require 2htdp/universe)

;; a fun animation

;; =================
;; constants:

(define HEIGHT 1000)
(define WIDTH 1000)
(define MTS (empty-scene WIDTH HEIGHT "white"))
(define FUN (above (beside (square 40 "solid" "blue")
                           (square 40 "solid" "yellow"))
                   (beside (square 40 "solid" "yellow")
                           (square 40 "solid" "blue"))))
(define SIZE-INCREASE 0.025)

;; =================
;; data definitions:


(define-struct surprise (x y g))
;; surprise is (make-surprise Number Number Number)
;; (make-surprise x y g) is a surprise with
;; x is the x-coordinate of the surprise
;; y is the y-coordinate of the surprise
;; g is the growth factor of the surprise

(define surprise1 (make-surprise 500 500 0.025))
(define surprise2 (make-surprise 250 750 5))
(define surprise3 (make-surprise 125 375 30))
(define surprise4 (make-surprise 800 300 1))
(define surprise5 (make-surprise 3000 3000 10))
(define surprise6 (make-surprise 0 0 3))

#;
(define (fn-for-surprise s)
  (... (x s)   ; Number
       (y s)   ; Number
       (g s))) ; Number

;; template rules used:
;; - Compound: 3 fields

;; =================
;; functions:

;; surprise -> surprise
;; start the world with surprise
;; no stub for main
      
(define (main surprise)
  (big-bang surprise            ; surprise
    (on-mouse handle-mouse)     ; surprise Integer Integer MouseEvent -> surprise
    (on-tick grow-surprise)     ; surprise -> surprise
    (to-draw render-surprise))) ; surprise -> Image

;; surprise -> surprise
;; grow the surprise by the provided growth factor
;; (define (grow-surprise s) 0)

(check-expect (grow-surprise surprise1) (make-surprise (surprise-x surprise1) (surprise-y surprise1) (+ (surprise-g surprise1) SIZE-INCREASE)))
(check-expect (grow-surprise surprise2) (make-surprise (surprise-x surprise2) (surprise-y surprise2) (+ (surprise-g surprise2) SIZE-INCREASE)))
(check-expect (grow-surprise surprise3) (make-surprise (surprise-x surprise3) (surprise-y surprise3) (+ (surprise-g surprise3) SIZE-INCREASE)))
(check-expect (grow-surprise surprise4) (make-surprise (surprise-x surprise4) (surprise-y surprise4) (+ (surprise-g surprise4) SIZE-INCREASE)))

;; template taken from the data definition

(define (grow-surprise s)
  (make-surprise (surprise-x s) (surprise-y s) (+ (surprise-g s) SIZE-INCREASE)))
  
;; surprise -> Image
;; render surprise 
;; (define (render-surprise s) 0)

(check-expect (render-surprise surprise1) (place-image (scale (surprise-g surprise1) FUN) (surprise-x surprise1) (surprise-y surprise1) MTS))
(check-expect (render-surprise surprise2) (place-image (scale (surprise-g surprise2) FUN) (surprise-x surprise2) (surprise-y surprise2) MTS))
(check-expect (render-surprise surprise3) (place-image (scale (surprise-g surprise3) FUN) (surprise-x surprise3) (surprise-y surprise3) MTS))
(check-expect (render-surprise surprise4) (place-image (scale (surprise-g surprise4) FUN) (surprise-x surprise4) (surprise-y surprise4) MTS))

;; template taken from the data definition

(define (render-surprise s)
  (place-image (scale (surprise-g s) FUN) (surprise-x s) (surprise-y s) MTS))

;; surprise Integer Integer MouseEvent -> surprise
;; set the coordinates of the surprise according to where the mouse is clicked
;; no stub for imported template

#;
(define (handle-mouse s x y me)
  (cond [(mouse=? me "button-down") (... s x y)]
        [else
         (... s x y)]))

(define (handle-mouse s x y me)
  (cond [(mouse=? me "button-down") (make-surprise x y SIZE-INCREASE)]
        [else s]))

(main surprise1)