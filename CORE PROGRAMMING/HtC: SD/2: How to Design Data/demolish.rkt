;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname demolish) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; You are assigned to develop a system that will classify buildings in downtown Vancouver based on how old they are. 
; According to city guidelines, there are three different classification levels: new, old, and heritage.
; Design a data definition to represent these classification levels and call it BuildingStatus.

; A possible structure definition (not until compound data)
; A type comment that defines a new type name and describes how to form data of that type.
; An interpretation that describes the correspondence between information and data.
; One or more examples of the data.
; A template for a 1 argument function operating on data of this type.

; type:
; BuildingStatus is one of:
; - new
; - old
; - heritage
; interpretation: building is either new, old or heritage (historic)

; examples are redundant in this case because it's an enumeration

#; template:
(define (fn-for-BuildingStatus b)
  (cond [(string=? "new" b) (...)]
        [(string=? "old" b) (...)]
        [(string=? "heritage" b) (...)]))

; template rules used:
; - one of: 3 cases
; - atomic non-distinct: "new"
; - atomic non-distinct: "old"
; - atomic non-distinct: "heritage"

; The city wants to demolish all buildings classified as "old". 
; You are hired to design a function called demolish? that determines whether a building should be torn down or not.

; Signature, purpose and stub.
; Define examples, wrap each in check-expect.
; Template and inventory.
; Code the function body.
; Test and debug until correct

; signature: BuildingStatus -> Boolean
; purpose: to determine whether we should demolish a building based on its age
; stub: (define (demolish? b) 0)

(check-expect (demolish? "new") false)
(check-expect (demolish? "old") true)
(check-expect (demolish? "heritage") false)

; template taken from the data definition

(define (demolish? b)
  (cond [(string=? "new" b) false]
        [(string=? "old" b) true]
        [(string=? "heritage" b) false]))

(define Discoteca-Alcatraz "new")
(define Teatro-Romano "old")
(define Duomo-di-Milano "heritage")

(demolish? Discoteca-Alcatraz)
(demolish? Teatro-Romano)
(demolish? Duomo-di-Milano)