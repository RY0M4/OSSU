;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |traffic light|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Design an animation of a traffic light. 

(require 2htdp/image)
(require 2htdp/universe)

;; a traffic light animation

;; =================
;; constants:

(define HEIGHT 200)
(define WIDTH 200)
(define MTS (empty-scene WIDTH HEIGHT "black"))
(define X 100)
(define Y 100)
(define SIZE 15)
(define SPACE (rectangle 10 10 "solid" "black"))
(define LIGHT-RED (place-image (above (circle SIZE "solid" "red")
                                      SPACE
                                      (circle SIZE "outline" "yellow")
                                      SPACE
                                      (circle SIZE "outline" "green")) X Y MTS))

(define LIGHT-YELLOW (place-image (above (circle SIZE "outline" "red")
                                         SPACE
                                         (circle SIZE "solid" "yellow")
                                         SPACE
                                         (circle SIZE "outline" "green")) X Y MTS))

(define LIGHT-GREEN (place-image (above (circle SIZE "outline" "red")
                                        SPACE
                                        (circle SIZE "outline" "yellow")
                                        SPACE
                                        (circle SIZE "solid" "green")) X Y MTS))
                                                  
;; =================
;; data definitions:

;; light is one of:
;; - "red"
;; - "yellow"
;; - "greem"
;; the light is either green, yellow or red
;; examples are redundant for enumerations

#;
(define (fn-for-light light)
  (cond [(string=? light "red") (...)]
        [(string=? light "yellow") (...)]
        [(string=? light "green") (...)]))

;; template rules used:
;; - one of: 3 cases
;; - atomic distinct: "red"
;; - atomic distinct: "yellow"
;; - atomic distinct: "green"

;; =================
;; functions:

;; light -> light
;; start the world with light
;; no stub for main

(define (main light)
  (big-bang light              ; light
    (to-draw render)           ; light -> Image
    (on-tick change-color 1))) ; light -> light

;; light -> light
;; produce the next light in the sequence
;; (define (change-color light) 0)

(check-expect (change-color "red") "yellow")
(check-expect (change-color "yellow") "green")
(check-expect (change-color "green") "red")

;; template taken from the data definition

(define (change-color light)
  (cond [(string=? light "red") "yellow"]
        [(string=? light "yellow") "green"]
        [(string=? light "green") "red"]))

;; light -> Image
;; render light
;; (define (render light) 0)

(check-expect (render "red") LIGHT-RED)
(check-expect (render "yellow") LIGHT-YELLOW)
(check-expect (render "green") LIGHT-GREEN)

;; template taken from the data definition

(define (render light)
  (cond [(string=? light "red") LIGHT-RED]
        [(string=? light "yellow") LIGHT-YELLOW]
        [(string=? light "green") LIGHT-GREEN]))

(main "red")
(main "yellow")
(main "green")
