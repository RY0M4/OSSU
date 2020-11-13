;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname water-balloon) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; In this problem, we will design an animation of throwing a water balloon.  
;; When the program starts the water balloon should appear on the left side of the screen, half-way up.
;; Since the balloon was thrown, it should fly across the screen, rotating in a clockwise fashion.
;; Pressing the space key should cause the program to start over with the water balloon back at the left side of the screen.

(require 2htdp/image)
(require 2htdp/universe)

;; a water balloon animation

;; =================
;; constants:

(define HEIGHT 500)
(define WIDTH 500)
(define SPEED 10)
(define ANGULAR-SPEED 10)
(define Y 250)
(define MTS (empty-scene WIDTH HEIGHT "white"))
(define BALLOON (circle 50 "solid" "blue")) ; replace this with the balloon image provided in the repository

;; =================
;; data definitions:

(define-struct balloon-throw (x a))
;; balloon-throw is (make-balloon-throw Number Number)
;; (make-balloon-throw x v) is a balloon-throw with
;; x is the x-coordinate of the balloon
;; a is the angle of the throw

(define balloon-throw1 (make-balloon-throw 0 45))
(define balloon-throw2 (make-balloon-throw 100 90))
(define balloon-throw3 (make-balloon-throw 300 180))
(define balloon-throw4 (make-balloon-throw 25 0))

#;
(define (fn-for-ballon-throw b-t)
  (... (x b-t)   ; Number
       (a b-t))) ; Number

;; template rules used:
;; - Compound: 2 fields

;; =================
;; functions:

;; balloon-throw -> balloon-throw
;; start the world with balloon-throw
;; no stub for main

(define (main balloon-throw)
  (big-bang balloon-throw    ; balloon-throw
    (to-draw render-b-t)     ; balloon-throw -> Image
    (on-tick advance-b-t)    ; balloon-throw -> balloon-throw
    (on-key handle-key)))    ; balloon-throw KeyEvent -> balloon-throw

;; balloon-throw -> balloon-throw
;; increase the x-coordinate by the linear speed and decrease the angle by the angular speed
;; (define (advance-b-t b-t) 0)

(check-expect (advance-b-t balloon-throw1) (make-balloon-throw (+ (balloon-throw-x balloon-throw1) SPEED) (- (balloon-throw-a balloon-throw1) ANGULAR-SPEED)))
(check-expect (advance-b-t balloon-throw2) (make-balloon-throw (+ (balloon-throw-x balloon-throw2) SPEED) (- (balloon-throw-a balloon-throw2) ANGULAR-SPEED)))
(check-expect (advance-b-t balloon-throw3) (make-balloon-throw (+ (balloon-throw-x balloon-throw3) SPEED) (- (balloon-throw-a balloon-throw3) ANGULAR-SPEED)))
(check-expect (advance-b-t balloon-throw4) (make-balloon-throw (+ (balloon-throw-x balloon-throw4) SPEED) (- (balloon-throw-a balloon-throw4) ANGULAR-SPEED)))

;; template taken from the data definition

(define (advance-b-t b-t)
  (make-balloon-throw (+ (balloon-throw-x b-t) SPEED) (- (balloon-throw-a b-t) ANGULAR-SPEED)))

;; balloon-throw -> Image
;; render the balloon throw 
;; (define (render-b-t b-t) 0)

(check-expect (render-b-t balloon-throw1) (place-image (rotate (modulo (balloon-throw-a balloon-throw1) 360) BALLOON) (balloon-throw-x balloon-throw1) Y MTS))
(check-expect (render-b-t balloon-throw2) (place-image (rotate (modulo (balloon-throw-a balloon-throw2) 360) BALLOON) (balloon-throw-x balloon-throw2) Y MTS))
(check-expect (render-b-t balloon-throw3) (place-image (rotate (modulo (balloon-throw-a balloon-throw3) 360) BALLOON) (balloon-throw-x balloon-throw3) Y MTS))
(check-expect (render-b-t balloon-throw4) (place-image (rotate (modulo (balloon-throw-a balloon-throw4) 360) BALLOON) (balloon-throw-x balloon-throw4) Y MTS))

;; template taken from the data definition

(define (render-b-t b-t)
  (place-image (rotate (modulo (balloon-throw-a b-t) 360) BALLOON) (balloon-throw-x b-t) Y MTS))

;; balloon-throw KeyEvent -> balloon-throw
;; reset the balloon throw animation when the key spacebar is pressed
;; no stub for imported template

(check-expect (handle-key balloon-throw1 " ") (make-balloon-throw 0 0))
(check-expect (handle-key balloon-throw2 "a") (make-balloon-throw (balloon-throw-x balloon-throw2) (balloon-throw-a balloon-throw2)))
(check-expect (handle-key balloon-throw3 "_") (make-balloon-throw (balloon-throw-x balloon-throw3) (balloon-throw-a balloon-throw3)))
(check-expect (handle-key balloon-throw4 "3") (make-balloon-throw (balloon-throw-x balloon-throw4) (balloon-throw-a balloon-throw4)))

#; 
(define (handle-key b-t key)
  (cond [(key=? key " ") (... b-t)]
        [else 
         (... b-t)]))

(define (handle-key b-t key)
  (cond [(key=? key " ") (make-balloon-throw 0 0)]
        [else
         (make-balloon-throw (balloon-throw-x b-t) (balloon-throw-a b-t))]))

(main balloon-throw1)
(main balloon-throw2)
(main balloon-throw3)
(main balloon-throw4)
