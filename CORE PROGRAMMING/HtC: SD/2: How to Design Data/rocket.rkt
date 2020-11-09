;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname rocket) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; You are designing a program to track a rocket's journey as it descends 100 kilometers to Earth.
; You are only interested in the descent from 100 kilometers to touchdown. Once the rocket has landed it is done.
; Design a data definition to represent the rocket's remaining descent and call it RocketDescent.

; A possible structure definition (not until compound data)
; A type comment that defines a new type name and describes how to form data of that type.
; An interpretation that describes the correspondence between information and data.
; One or more examples of the data.
; A template for a 1 argument function operating on data of this type.

; type:
; RocketDescent is one of:
; - Number[0, 100]
; - false
; interpretation: the number of kilometers remaining till landing or false if it has landed
; data examples: start, middle, near the end and end

(define n1 100)
(define n2 50)
(define n3 0.5)
(define n4 false)

#; template:
(define (fn-for-RocketDescent n)
  (cond [(and (number? n)
              (<= 0 n)
              (< n 10))
         (... n)]
        [else (...)]))

; template rules used:
; - one of: 2 cases
; - atomic non-distinct: Number[0, 100]
; - atomic distinct: false

; Design a function called rocket-descent-to-msg that will output the rocket's remaining descent distance in a short string that can be broadcast on Twitter. 
; When the descent is over, the message should be "The rocket has landed!".

; Signature, purpose and stub.
; Define examples, wrap each in check-expect.
; Template and inventory.
; Code the function body.
; Test and debug until correct

; signature: RocketDescent -> String
; purpose: to return the rocket's remaining descent distance in a message format
; stub: (define (rocket-descent-to-msg n) "")

(check-expect (rocket-descent-to-msg 100) "The remaining descent distance is 100 kilometers.")
(check-expect (rocket-descent-to-msg 50) "The remaining descent distance is 50 kilometers.")
(check-expect (rocket-descent-to-msg 0.5) "The remaining descent distance is 1/2 kilometers.")
(check-expect (rocket-descent-to-msg false) "The rocket has landed!")

; template taken from the data definition

(define (rocket-descent-to-msg n)
  (cond [(and (number? n)
              (< 0 n)
              (<= n 100))
         (string-append "The remaining descent distance is " (number->string n) " kilometers.")]
        [else "The rocket has landed!"]))

(rocket-descent-to-msg n1)
(rocket-descent-to-msg n2)
(rocket-descent-to-msg n3)
(rocket-descent-to-msg n4)
  
  
  




