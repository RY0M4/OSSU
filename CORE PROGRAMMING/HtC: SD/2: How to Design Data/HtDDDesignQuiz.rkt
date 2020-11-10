;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname demolish) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Consider the following data definition for the age of a person.

; type:
; Age is Natural
; interpretation: the age of a person in years

(define A0 18)
(define A1 25)

#; template:
(define (fn-for-age a)
  (... a))

; template rules used:
; - atomic non-distinct: Natural

; Design a function called teenager? that determines whether a person of a particular age is a teenager (between the ages of 13 and 19.)

; Signature, purpose and stub.
; Define examples, wrap each in check-expect.
; Template and inventory.
; Code the function body.
; Test and debug until correct

; signature: Age -> Boolean
; purpose: to determine if a person is a teenager
; stub: (define (teenager? A) 0)

(check-expect (teenager? A0) true)
(check-expect (teenager? A1) false)

; template taken from the data definition

(define (teenager? A)
  (and (number? A)
       (<= A 18)
       (>= A 13)))

(teenager? 16)
(teenager? 32)

; Design a data definition called MonthAge to represent a person's age in months.

; A possible structure definition (not until compound data)
; A type comment that defines a new type name and describes how to form data of that type.
; An interpretation that describes the correspondence between information and data.
; One or more examples of the data.
; A template for a 1 argument function operating on data of this type.

; type:
; MonthAge is Natural
; interpretation: represent a person's age in months
; examples: 0 years, 10 years, 20 years, 30 years

(define M0 0)
(define M1 10)
(define M2 20)
(define M3 30)

#; template:
(define (fn-for-MonthAge M)
  (... M))

; template rules used:
; - atomic non-distinct: Natural

; Design a function called months-old that takes a person's age in years and yields that person's age in months.

; signature: MonthAge -> Natural
; purpose: to return the age of a person from years to months
; stub: (define (months-old M) 0)

(check-expect (months-old 0) 0)
(check-expect (months-old 1) 12)
(check-expect (months-old 2) 24)
(check-expect (months-old 45) 540)

; template taken from the data definition

(define (months-old M)
  (* M 12))

(months-old M0)
(months-old M1)
(months-old M2)
(months-old M3)

; Consider a video game where you need to represent the health of your character.
; The only thing that matters about their health is:
; - if they are dead (which is shockingly poor health)
; - if they are alive then they can have 0 or more extra lives

; Design a data definition called Health to represent the health of your character.

; type:
; Health is one of:
; - "dead"
; - Natural
; interpretation: the status of the health of the character
; examples: dead, 0 lives, few lives and a lots of lives

(define H0 "dead")
(define H1 0)
(define H2 3)
(define H3 10)

#; template:
(define (fn-for-Health H)
  (cond [(string? H) (... H)]
        [(number? H) (... H)]))
                      
; template rules used:
; - atomic non-distinct: "dead"
; - atomic non-distinct: Natural

; Design a function called increase-health that allows you to increase the lives of a character.
; The function should only increase the lives of the character if the character is not dead, otherwise the character remains dead.

; signature: Health -> Health
; purpose: to increase the lives of a character (if they're not dead)
; stub: (define (increase-health H) 0)

(check-expect (increase-health "dead") "dead")
(check-expect (increase-health 0) 1)
(check-expect (increase-health 1) 2)
(check-expect (increase-health 5) 6)

; template taken from the data definition

(define (increase-health H)
  (cond [(string? H) "dead"]
        [(number? H) (+ H 1)]))
  
(increase-health H0)
(increase-health H1)
(increase-health H2)
(increase-health H3)