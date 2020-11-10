;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname HtDDDesignQuiz) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Consider a video game where you need to represent the health of your character.
; The only thing that matters about their health is:
; - if they are dead (which is shockingly poor health)
; - if they are alive then they can have 0 or more extra lives

; Design a data definition called Health to represent the health of your character.

; type:
; Health is Integer
; interpretation: the status of the health of the character
; examples: dead, 0 lives, few lives and lots of lives

(define H0 -1)
(define H1 0)
(define H2 3)
(define H3 10)

#; template:
(define (fn-for-Health H)
  (... H))
                      
; template rules used:
; - atomic non-distinct: Integer

; Design a function called increase-health that allows you to increase the lives of a character.
; The function should only increase the lives of the character if the character is not dead, otherwise the character remains dead.

; signature: Health -> Health
; purpose: to increase the lives of a character (if they're not dead)
; stub: (define (increase-health H) 0)

(check-expect (increase-health -1) -1)
(check-expect (increase-health 0) 1)
(check-expect (increase-health 1) 2)
(check-expect (increase-health 5) 6)

; template taken from the data definition

(define (increase-health H)
  (if (>= H 0) (+ H 1) (+ H 0)))

(increase-health H0)
(increase-health H1)
(increase-health H2)
(increase-health H3)
