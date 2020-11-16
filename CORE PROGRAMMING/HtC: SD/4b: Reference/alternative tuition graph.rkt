;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |alternative tuition graph|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Consider the following type comment for a school tuition information program.
;; Complete the data definition making sure to define all the same examples as for ListOfSchool in the videos.
;; Design the chart function that consumes School.
;; Save yourself time by simply copying the tests over from the original version of chart.

(require 2htdp/image)

(define-struct school (name tuition next))
;; School is one of:
;;  - false
;;  - (make-school String Natural School)
;; school is an arbitrary number of schools, where for each school we have its name and its tuition in USD

(define S0 false)
(define S1 (make-school "MIT" 100000 (make-school "UBC" 50000 (make-school "UW" 25000 false))))

#;
(define (fn-for-school s)
  (cond [(false? s) (...)]
        [else
         (... (school-name s)
         (school-tuition s)
         ((fn-for-school (school-next s))))]))

;; template rules used:
;; - atomic distinct: false
;; - compound: (make-school String Natural School)
;; - atomic non-distinct: (school-name s) is String
;; - atomic non-distinct: (school-tuition s) is Natural
;; - self-reference: (school-next s) is School

;; constants:

(define FONT-SIZE 24)
(define FONT-COLOR "black")
(define Y-SCALE   1/200)
(define BAR-WIDTH 30)
(define BAR-COLOR "lightblue")

;; School -> Image
;; to produce a bar chart showing the names and the tuition of the schools
;; (define (chart s) 0)

(check-expect (chart false) (square 0 "solid" "white"))
(check-expect (chart (make-school "S1" 8000 false))
              (beside/align "bottom"
                            (overlay/align "center" "bottom"
                                           (rotate 90 (text "S1" FONT-SIZE FONT-COLOR))
                                           (rectangle BAR-WIDTH (* 8000 Y-SCALE) "outline" "black")
                                           (rectangle BAR-WIDTH (* 8000 Y-SCALE) "solid" BAR-COLOR))
                            (square 0 "solid" "white")))
(check-expect (chart (make-school "S2" 12000 (make-school "S1" 8000 false))) 
              (beside/align "bottom"
                            (overlay/align "center" "bottom"
                                           (rotate 90 (text "S2" FONT-SIZE FONT-COLOR))
                                           (rectangle BAR-WIDTH (* 12000 Y-SCALE) "outline" "black")
                                           (rectangle BAR-WIDTH (* 12000 Y-SCALE) "solid" BAR-COLOR))
                            (overlay/align "center" "bottom"
                                           (rotate 90 (text "S1" FONT-SIZE FONT-COLOR))
                                           (rectangle BAR-WIDTH (* 8000 Y-SCALE) "outline" "black")
                                           (rectangle BAR-WIDTH (* 8000 Y-SCALE) "solid" BAR-COLOR))
                            (square 0 "solid" "white")))

;; template taken from the data definition

(define (chart s)
  (cond [(false? s) (square 0 "solid" "white")]
        [else
         (beside/align "bottom"
                       (overlay/align "center" "bottom"
                                      (rotate 90 (text (school-name s) FONT-SIZE FONT-COLOR))
                                      (rectangle BAR-WIDTH (* (school-tuition s) Y-SCALE) "outline" "black")
                                      (rectangle BAR-WIDTH (* (school-tuition s) Y-SCALE) "solid" BAR-COLOR))
         (chart (school-next s)))]))