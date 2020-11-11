;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Countdown) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Design an animation of a simple countdown. 
; Your program should display a simple countdown, that starts at ten, and decreases by one each clock tick until it reaches zero, and stays there.
; To make your countdown progress at a reasonable speed, you can use the rate option to on-tick.
; If you say, for example, (on-tick advance-countdown 1) then big-bang will wait 1 second between calls to advance-countdown.
; Remember to follow the HtDW recipe! Be sure to do a proper domain analysis before starting to work on the code file.
; Once you are finished the simple version of the program, you can improve it by reseting the countdown to ten when you press the spacebar.

(require 2htdp/image)
(require 2htdp/universe)

;; a countdown animation

;; =================
;; constants:

(define HEIGHT 100)
(define WIDTH 100)
(define X 50)
(define Y 50)
(define MTS (empty-scene WIDTH HEIGHT))
(define SIZE 50)
(define COLOR "black")

;; =================
;; data definitions:

;; countdown is Natural[0, 10]
;; it counts down from 10 and it stops at 0

(define T0 10) ; start
(define T1 5)  ; middle
(define T2 0)  ; end

#;
(define (fn-for-countdown countdown)
  (... countdown))

;; template rules used:
;; - atomic non-distinct: Natural[0, 10]

;; =================
;; Functions:

;; countdown -> countdown
;; start the world with countdown
;; no stub for main

(define (main countdown)
  (big-bang countdown             ; countdown
    (to-draw render)              ; countdown -> Image
    (on-tick advance-countdown 1) ; countdown -> countdown
    (on-key handle-key)))         ; countdown KeyEvent -> countdown

;; countdown -> countdown
;; advance the countdown by 1
;; (define (advance-countdown cd) 0)

(check-expect (advance-countdown 10) 9)
(check-expect (advance-countdown 5) 4)
(check-expect (advance-countdown 0) 0)

#;
(define (advance-countdown countdown)
  (... countdown))

(define (advance-countdown countdown)
  (if (> countdown 0) (- countdown 1) 0))

;; countdown -> Image
;; render countdown
;; (define (render countdown) 0)

(check-expect (render 10) (place-image (text (number->string 10) SIZE COLOR) X Y MTS))
(check-expect (render 5) (place-image (text (number->string 5) SIZE COLOR) X Y MTS))
(check-expect (render 0) (place-image (text (number->string 0) SIZE COLOR) X Y MTS))

#;
(define (render countdown)
  (... countdown))

(define (render countdown)
  (place-image (text (number->string countdown) SIZE COLOR) X Y MTS))

;; countdown KeyEvent -> countdown
;; reset the countdown to 10 when the key spacebar is pressed
;; no stub for imported template

(check-expect (handle-key 10 " ") 10)
(check-expect (handle-key 5 " ") 10)
(check-expect (handle-key 0 " ") 10)
(check-expect (handle-key 8 "a") 8)
(check-expect (handle-key 3 "_") 3)
(check-expect (handle-key 1 "3") 1)

#; 
(define (handle-key ws key)
  (cond [(key=? key " ") (... ws)]
        [else 
         (... ws)]))

(define (handle-key countdown key)
  (cond [(key=? key " ") (+ countdown (- 10 countdown))]
        [else
         (+ countdown 0)]))

(main T0)
(main T1)
(main T2)