;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |boolean list|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Design a data definition to represent a list of booleans and call it ListOfBoolean.

;; ListOfBoolean is one of:
;; - empty
;; - (cons Boolean ListOfBoolean)
;; ListOfBoolean is either empty or it contains a list of booleans

(define LOB1 empty)
(define LOB2 (cons true (cons false empty)))
(define LOB3 (cons true (cons true empty)))

#;
(define (fn-for-lob lob)
  (cond [(empty? lob) (...)]
        [else
         (... (first lob)
              (fn-for-lob (rest lob)))]))

;; template rules used:
;; - one of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons Boolean ListOfBoolean)
;; - self-reference: (rest lob) is ListOfBoolean

;; Design a function called all-true? that consumes a list of boolean values and produces true if every value in the list is true.
;; If the list is empty, your function should also produce true.

;; ListOfBoolean -> Boolean
;; to return true if all the values in the list are true or if the list is empty
;; (define (all-true? lob) 0)

(check-expect (all-true? LOB1) true)
(check-expect (all-true? LOB2) false)
(check-expect (all-true? LOB3) true)

;; template taken from the data definition

(define (all-true? lob)
  (cond [(empty? lob) true]
        [else
         (if (and (first lob)
                  (all-true? (rest lob)))
             true
             false)]))