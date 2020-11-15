;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |image list|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Design a data definition to represent a list of images and call it ListOfImage.

(require 2htdp/image)

;; ListOfImage is one of:
;; - empty
;; - (cons Image ListOfImage)
;; ListOfImage is either empty or it contains a list of images

(define LOI1 empty)
(define LOI2 (cons (circle 25 "solid" "blue") (cons (square 25 "solid" "red") empty)))
(define LOI3 (cons (ellipse 15 40 "solid" "yellow") (cons (rectangle 25 50 "solid" "orange") empty)))

#;
(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else
         (... (first loi)
              (fn-for-loi (rest loi)))]))

;; template rules used:
;; - one of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons Image ListOfImage)
;; - self-reference: (rest loi) is ListOfImage

;; Design a function that consumes a list of images and produces a number that is the sum of the areas of each image.
;; For area, just use the image's width times its height.

;; ListOfImage -> Number
;; to return the sum of the areas of each image in the list
;; (define (sum-area loi) 0)

(check-expect (sum-area LOI1) 0)
(check-expect (sum-area LOI2) 3125)
(check-expect (sum-area LOI3) 1850)

;; template taken from the data definition

(define (sum-area loi)
  (cond [(empty? loi) 0]
        [else
         (+ (* (image-width (first loi)) (image-height (first loi)))
            (sum-area (rest loi)))]))
