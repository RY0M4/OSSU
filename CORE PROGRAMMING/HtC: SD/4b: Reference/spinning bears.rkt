;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |spinning bears|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; This world is about spinning bears.
;; The world will start with an empty screen.
;; Clicking anywhere on the screen will cause a bear to appear at that spot.
;; The bear starts out upright, but then rotates counterclockwise at a constant speed.
;; Each time the mouse is clicked on the screen, a new upright bear appears and starts spinning.

(require 2htdp/image)
(require 2htdp/universe)

;; an animation about spinning bears

;; =================
;; constants:

(define WIDTH 1000)
(define HEIGHT 1000)
(define MTS (empty-scene WIDTH HEIGHT "forest green"))
(define ANGULAR-SPEED 5)
(define BEAR (above (triangle 25 "solid" "brown") (circle 25 "solid" "brown"))) ; replace this with the bear image provided in the repository 

;; =================
;; data definitions:

(define-struct bear (x y a))
;; bear is (make-bear Number Number Number)
;; (make-bear x y) is a bear with
;; x is the x-coordinate of the bear
;; y is the y-coordinate of the bear
;; a is the angle of the bear

(define B0 (make-bear 0 0 0))
(define B1 (make-bear (/ WIDTH 2) (/ HEIGHT 2) 0)) 

#;
(define (fn-for-bear b)
  (... (x b)
       (y b)
       (a b)))

;; template rules used:
;; - Compound: 3 fields

;; ListOfBear is one of:
;; - empty
;; - (cons bear ListOfBear)
;; ListOfBear is either empty or it contains bears

(define LOB0 empty)
(define LOB1 (cons B0 (cons B1 empty)))

#;
(define (fn-for-lob lob)
  (cond [(empty? lob) (...)]
        [else
         (... (first lob)
              (fn-for-lob (rest lob)))]))

;; template rules used:
;; - one of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons bear ListOfBear)
;; - reference: (first lob) is bear
;; - self-reference: (rest lob) is ListOfBear

;; =================
;; functions:

;; ListOfBear -> ListOfBear
;; start the world with ListOfBear
;; no stub for main

(define (main lob)
  (big-bang lob              ; ListOfBear
    (on-mouse handle-mouse)  ; ListOfBear Integer Integer MouseEvent -> ListOfBear
    (to-draw render-bears)   ; ListOfBear -> Image
    (on-tick rotate-bears))) ; ListOfBear -> ListOfBear

;; ListOfBear -> Image
;; render bears
;; (define (render-bears lob) 0)

(check-expect (render-bears LOB0) MTS)
(check-expect (render-bears LOB1) (place-image (rotate (bear-a B0) BEAR) (bear-x B0) (bear-y B0) (place-image (rotate (bear-a B1) BEAR) (bear-x B1) (bear-y B1) MTS)))

;; template taken from the data definition

(define (render-bears lob)
  (cond [(empty? lob) MTS]
        [else 
         (render-bear (first lob) (render-bears (rest lob)))]))

;; bear -> Image
;; render bear
;; (define (render-bear b i) 0)

(check-expect (render-bear B1 MTS) (place-image (rotate (bear-a B1) BEAR) (bear-x B1) (bear-y B1) MTS))

;; template taken from the data definition with an added atomic element (atomic distinct: Image)

(define (render-bear b i)
  (place-image (rotate (bear-a b) BEAR) (bear-x b) (bear-y b) i))

;; ListOfBear -> ListOfBear
;; to return the next positions of the bears
;; (define (rotate-bears lob) 0)

(check-expect (rotate-bears LOB0) empty)
(check-expect (rotate-bears LOB1) (cons (make-bear (bear-x B0) (bear-y B0) (+ (bear-a B0) ANGULAR-SPEED)) (cons (make-bear (bear-x B1) (bear-y B1) (+ (bear-a B1) ANGULAR-SPEED)) empty)))

;; template taken from the data definition

(define (rotate-bears lob)
  (cond [(empty? lob) empty]
        [else
         (cons (rotate-bear (first lob))
               (rotate-bears (rest lob)))]))

;; bear -> bear
;; to return the next position of the bear
;; (define (rotate-bear b) 0)

(check-expect (rotate-bear B1) (make-bear (bear-x B1) (bear-y B1) (+ (bear-a B1) ANGULAR-SPEED)))

;; template taken from the data definition

(define (rotate-bear b)
  (make-bear (bear-x b) (bear-y b) (+ (bear-a b) ANGULAR-SPEED)))

;; ListOfBear Integer Integer MouseEvent -> ListOfBear
;; set the coordinates of the bear according to where the mouse is clicked
;; no stub for imported template

#;
(define (handle-mouse lob x y me)
  (cond [(mouse=? me "button-down") (... lob x y)]
        [else
         (... lob x y)]))

(define (handle-mouse lob x y me)
  (cond [(mouse=? me "button-down") (cons (make-bear x y 0) lob)]
        [else lob]))

(main LOB0)