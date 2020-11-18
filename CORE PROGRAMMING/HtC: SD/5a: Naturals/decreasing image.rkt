;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |decreasing image|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;  Design a function called decreasing-image that consumes a Natural n and produces an image of all the numbers from n to 0 side by side.

(require 2htdp/image)

;; Natural is one of:
;; - 0
;; - (add1 Natural)
;; it is a natural number

(define N0 0)
(define N1 (add1 N0))
(define N2 (add1 N1))
(define N3 (add1 N2))

(define (fn-for-Natural n)
  (cond [(zero? n) (...)]
        [else
         (... n
              (fn-for-Natural (sub1 n)))]))

;; template rules used:
;; - one of: 2 cases
;; - atomic distinct: 0
;; - compound: (add1 Natural)
;; - self-reference: (sub1 n) is Natural

;; decreasing-image -> Image
;; to return an image of all the numbers from n to 0
;; (define (decreasing-image n) 0)

(check-expect (decreasing-image N0) (text "0" 50 "black"))
(check-expect (decreasing-image N1) (beside (text "1" 50 "black") (rectangle 50 10 "solid" "white") (text "0" 50 "black")))
(check-expect (decreasing-image N2) (beside (text "2" 50 "black") (rectangle 50 10 "solid" "white") (text "1" 50 "black") (rectangle 50 10 "solid" "white") (text "0" 50 "black")))

;; template taken from the data definition

(define (decreasing-image n)
  (cond [(zero? n) (text "0" 50 "black")]
        [else
         (beside (text (number->string n) 50 "black") (rectangle 50 10 "solid" "white")
                 (decreasing-image (sub1 n)))]))

(decreasing-image N3)
         