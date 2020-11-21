;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |making rain filtered|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Design a simple interactive animation of rain falling down a screen.
;; Wherever we click, a rain drop should be created and as time goes by it should fall.
;; Over time the drops will reach the bottom of the screen and "fall off".
;; You should filter these excess drops out of the world state - otherwise your program is continuing to tick and and draw them long after they are invisible.

(require 2htdp/image)
(require 2htdp/universe)

;; a simple rain animation

;; =================
;; constants:

(define WIDTH  1000)
(define HEIGHT 1000)
(define MTS (rectangle WIDTH HEIGHT "solid" "light blue"))
(define DROP (ellipse 4 8 "solid" "blue"))
(define SPEED 1)

;; =================
;; data definitions:

(define-struct drop (x y))
;; Drop is (make-drop Integer Integer)
;; a raindrop on the screen, with x and y coordinates.

(define D0 (make-drop 0 0))
(define D1 (make-drop 10 30))
(define D2 (make-drop (/ WIDTH 2) HEIGHT))

#;
(define (fn-for-drop d)
  (... (drop-x d) 
       (drop-y d)))

;; template Rules used:
;; - compound: 2 fields

;; ListOfDrop is one of:
;;  - empty
;;  - (cons Drop ListOfDrop)
;; a list of drops

(define LOD0 empty)
(define LOD1 (cons (make-drop 10 20) (cons (make-drop 3 6) empty)))
(define LOD2 (cons D2 empty))

#;
(define (fn-for-lod lod)
  (cond [(empty? lod) (...)]
        [else
         (... (fn-for-drop (first lod))
              (fn-for-lod (rest lod)))]))

;; Template Rules used:
;; - one of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons Drop ListOfDrop)
;; - reference: (first lod) is Drop
;; - self reference: (rest lod) is ListOfDrop

;; =================
;; functions:

;; ListOfDrop -> ListOfDrop
;; start rain program by evaluating (main empty)

(define (main lod)
  (big-bang lod
            (on-mouse handle-mouse)   ; ListOfDrop Integer Integer MouseEvent -> ListOfDrop
            (on-tick  next-drops)     ; ListOfDrop -> ListOfDrop
            (to-draw  render-drops))) ; ListOfDrop -> Image

;; ListOfDrop Integer Integer MouseEvent -> ListOfDrop
;; if mouse button clicked is "button-down" then add a new drop at that position
;; no stub for imported template

#;
(define (handle-mouse lod x y me)
  (cond [(mouse=? me "button-down") (... lod x y)]
        [else
         (... lod x y)]))

(define (handle-mouse lod x y me)
  (cond [(mouse=? me "button-down") (cons (make-drop x y) lod)]
        [else lod]))

;; ListOfDrop -> ListOfDrop
;; to return a filtered and ticked list of drops

(define (next-drops lod)
  (onscreen-drops (advance-drops lod)))

;; ListOfDrop -> ListOfDrop
;; to return a ticked list of drops
;; (define (advance-drops lod) empty)

(check-expect (advance-drops LOD0) empty)
(check-expect (advance-drops LOD1) (cons (make-drop 10 21) (cons (make-drop 3 7) empty)))

;; template taken from the data definition

(define (advance-drops lod)
  (cond [(empty? lod) empty]
        [else
         (cons (advance-drop (first lod))
               (advance-drops (rest lod)))]))

;; Drop -> Drop
;; changes the y-coordinate of the drop according to the defined speed
;; (define (next-drop d) 0)

(check-expect (advance-drop D0) (make-drop 0 1))
(check-expect (advance-drop D1) (make-drop 10 31))

;; template taken from the data definition

(define (advance-drop d)
  (make-drop (drop-x d)
             (+ (drop-y d) SPEED)))

;; ListOfDrop -> ListOfDrop
;; to return a filtered list of drops
;; (define (onscreen-drops lod) 0)

(check-expect (onscreen-drops LOD0) empty)
(check-expect (onscreen-drops LOD1) (cons (make-drop 10 20) (cons (make-drop 3 6) empty)))
(check-expect (onscreen-drops LOD2) empty)

;; template taken from the data definition

(define (onscreen-drops lod)
  (cond [(empty? lod) empty]
        [else
         (if (is-onscreen? (first lod)) (cons (first lod) (onscreen-drops (rest lod)))
             (onscreen-drops (rest lod)))]))

;; Drop -> Boolean
;; to determine whether a drop is onscreen
;; (define (is-onscreen? d) 0)

(check-expect (is-onscreen? D0) true)
(check-expect (is-onscreen? D1) true)
(check-expect (is-onscreen? D2) false)

;; template taken from the data definition

(define (is-onscreen? d)
  (< (drop-y d) HEIGHT))

;; ListOfDrop -> Image
;; render the drops onto MTS
;; (define (render-drops lod) MTS)

(check-expect (render-drops LOD0) MTS)
(check-expect (render-drops LOD1) (place-image DROP 10 20 (place-image DROP 3 6 MTS)))

;; template taken from the data definition

(define (render-drops lod)
  (cond [(empty? lod) MTS]
        [else
         (render-drop (first lod) (render-drops (rest lod)))]))

;; Drop -> Image
;; render the drop
;; (define (render-drop d i) MTS)

(check-expect (render-drop D0 MTS) (place-image DROP 0 0 MTS))
(check-expect (render-drop D1 MTS) (place-image DROP 10 30 MTS))

;; template taken from the data definition

(define (render-drop d i)
  (place-image DROP (drop-x d) (drop-y d) i))

(main LOD2)