;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Naturals and Helpers Design Quiz|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Complete the design of a function called pyramid that takes a natural number n and an image, and constructs an n-tall, n-wide pyramid of copies of that image.

(require 2htdp/image)

;; a pyramid made of cookies

;; ======================================================================
;; constants

(define COOKIES (circle 25 "solid" "brown")) ; replace this with the bear image provided in the repository 

;; ======================================================================
;; data definitions

;; Natural is one of:
;; - 0
;; - (add1 Natural)
;; a natural number

(define N0 0)         ;0
(define N1 (add1 N0)) ;1
(define N2 (add1 N1)) ;2

#;
(define (fn-for-natural n)
  (cond [(zero? n) (...)]
        [else
         (... n                     
              (fn-for-natural (sub1 n)))]))

;; template rules used:
;; - one-of: two cases
;; - atomic distinct: 0
;; - compound: 2 fields
;; - self-reference: (sub1 n) is Natural

;; Natural Image -> Image
;; produces an n-wide pyramid of the given image
;; (define (pyramid n i) empty-image)

(check-expect (pyramid 0 COOKIES) empty-image)
(check-expect (pyramid 1 COOKIES) COOKIES)
(check-expect (pyramid 3 COOKIES)
              (above COOKIES
                     (beside COOKIES COOKIES)
                     (beside COOKIES COOKIES COOKIES)))

(define (pyramid n i)
  (cond [(zero? n) empty-image]
        [else
         (above (pyramid (sub1 n) i)
                (row n i))]))

;; Natural Image -> Image
;; to return a row of images based on n
;; (define (row n i) 0)

(check-expect (row 0 COOKIES) empty-image)
(check-expect (row 1 COOKIES) (beside COOKIES empty-image))
(check-expect (row 3 COOKIES) (beside COOKIES COOKIES COOKIES))

;; template taken from the data definition

(define (row n i)
  (cond [(zero? n) empty-image]
        [else
         (beside i (row (sub1 n) i))]))

(pyramid 12 COOKIES)
(pyramid 24 COOKIES)
(pyramid 36 COOKIES)
(pyramid 48 COOKIES)

;; Complete the design of a function that takes a list of blobs and sinks each solid blob by one.
;; It's okay to assume that a solid blob sinks past any neighbor just below it.

;; Blob is one of:
;; - "solid"
;; - "bubble"
;; a gelatinous blob, either a solid or a bubble
;; examples are redundant for enumerations

#;
(define (fn-for-blob b)
  (cond [(string=? b "solid") (...)]
        [(string=? b "bubble") (...)]))

;; template rules used:
;; - one of: 2 cases
;; - atomic distinct: "solid"
;; - atomic distinct: "bubble"

;; ListOfBlob is one of:
;; - empty
;; - (cons Blob ListOfBlob)
;; a sequence of blobs in a test tube, listed from top to bottom.

(define LOB0 empty)                                ; empty test tube
(define LOB1 (cons "solid" (cons "bubble" empty))) ; solid blob above a bubble

#;
(define (fn-for-lob lob)
  (cond [(empty? lob) (...)]
        [else
         (... (fn-for-blob (first lob))
              (fn-for-lob (rest lob)))]))

;; template rules used
;; - one of: 2 cases
;; - atomic distinct: empty
;; - compound: 2 fields
;; - reference: (first lob) is Blob
;; - self-reference: (rest lob) is ListOfBlob

;; ListOfBlob -> ListOfBlob
;; to produce a list of blobs that sinks the given solid blobs by one
;; (define (sink lob) empty)

(check-expect (sink empty) empty)
(check-expect (sink (cons "bubble" (cons "solid" (cons "bubble" empty))))
              (cons "bubble" (cons "bubble" (cons "solid" empty))))
(check-expect (sink (cons "solid" (cons "solid" (cons "bubble" empty))))
              (cons "bubble" (cons "solid" (cons "solid" empty))))
(check-expect (sink (cons "solid" (cons "bubble" (cons "bubble" empty))))
              (cons "bubble" (cons "solid" (cons "bubble" empty))))
(check-expect (sink (cons "solid" (cons "bubble" (cons "solid" empty))))
              (cons "bubble" (cons "solid" (cons "solid" empty))))
(check-expect (sink (cons "bubble" (cons "solid" (cons "solid" empty))))
              (cons "bubble" (cons "solid" (cons "solid" empty))))
(check-expect (sink (cons "solid" (cons "solid" (cons "bubble" (cons "bubble" empty)))))
              (cons "bubble" (cons "solid" (cons "solid" (cons "bubble" empty)))))

;; template taken from the data definition

(define (sink lob)
  (cond [(empty? lob) empty]
        [else
         (move-blob (first lob)
         (sink (rest lob)))]))

;; Blob ListOfBlob -> ListOfBlob
;; to return the new position of the blob in the list
;; (define (move-blob b lob) 0)

(check-expect (move-blob "solid" LOB0) (cons "solid" empty))
(check-expect (move-blob "solid" LOB1) (cons (first LOB1) (cons "solid" (rest LOB1))))
(check-expect (move-blob "bubble" LOB1) (cons "bubble" LOB1))

;; template taken from the data definition

(define (move-blob b lob)
  (cond [(empty? lob) (cons b empty)]
        [else
         (if (is-solid? b) (cons (first lob) (cons b (rest lob))) (cons b lob))]))

;; Blob -> Blob
;; to return true if the blob is a solid 
;; (define (is-solid? b) 0)

(check-expect (is-solid? "solid") true)
(check-expect (is-solid? "bubble") false)

;; template taken from the data definition

(define (is-solid? b)
  (cond [(string=? b "solid") true]
        [(string=? b "bubble") false]))


