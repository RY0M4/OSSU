;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname largest) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Observe the data definition for a list of numbers we designed in Lecture 5f.

;; ListOfNumber is one of:
;;  - empty
;;  - (cons Number ListOfNumber)
;; ListOfNumber is a list of numbers

(define LON1 empty)
(define LON2 (cons 60 (cons 42 empty)))
(define LON3 (cons 50 (cons 32 (cons 90 (cons 72 empty)))))
(define LON4 (cons 22 (cons 40 (cons 58 (cons 84 empty)))))

#;
(define (fn-for-lon lon)
  (cond [(empty? lon) (...)]
        [else
         (... (first lon)
              (fn-for-lon (rest lon)))]))

;; template rules used:
;;  - one of: 2 cases
;;  - atomic distinct: empty
;;  - compound: (cons Number ListOfNumber)
;;  - self-reference: (rest lon) is ListOfNumber

;; Design a function that consumes a list of numbers and produces the largest number in the list.
;; You may assume that all numbers in the list are greater than 0.
;; If the list is empty, produce 0.

;; ListOfNumber -> Number
;; to return the largest number in a list of numbers
;; (define (largest? lon) 0)

(check-expect (largest? LON1) 0)
(check-expect (largest? LON2) 60)
(check-expect (largest? LON3) 90)
(check-expect (largest? LON4) 84)

;; template taken from the data definition

(define (largest? lon)
  (cond [(empty? lon) 0]
        [else
         (if (> (first lon)
                (largest? (rest lon)))
             (first lon)
             (largest? (rest lon)))]))