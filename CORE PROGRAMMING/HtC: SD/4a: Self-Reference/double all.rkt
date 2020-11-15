;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |double all|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Observe the data definition for a list of numbers we designed in Lecture 5f.

;; ListOfNumber is one of:
;;  - empty
;;  - (cons Number ListOfNumber)
;; a list of numbers

(define LON1 empty)
(define LON2 (cons 60 (cons 42 empty)))

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

;; Design a function called double-all that consumes a list of numbers and doubles every number in the list.

;; ListOfNumber -> ListOfNumber
;; to return a list with each of its values doubled
;; (define (double-all lon) 0)

(check-expect (double-all LON1) empty)
(check-expect (double-all LON2) (cons 120 (cons 84 empty)))

;; template taken from the data definition

(define (double-all lon)
  (cond [(empty? lon) empty]
        [else
         (cons (* (first lon) 2)
               (double-all (rest lon)))]))
