;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |pattern match|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Now design a function that consumes Pattern and ListOf1String and produces true if the pattern matches the ListOf1String.

;; =================
;; data definitions:

;; 1String is String
;; these are strings that are only 1 character long

(define 1SA "x")
(define 1SB "2")

;; Pattern is one of:
;; - empty
;; - (cons "A" Pattern)
;; - (cons "N" Pattern)
;; a pattern describing certain ListOf1String 
;; "A" means the corresponding letter must be alphabetic
;; "N" means it must be numeric  
;; (list "A" "N" "A" "N" "A" "N") describes Canadian postal codes like (list "V" "6" "T" "1" "Z" "4")

(define PATTERN1 (list "A" "N" "A" "N" "A" "N"))

;; ListOf1String is one of:
;; - empty
;; - (cons 1String ListOf1String)
;; a list of strings each 1 character long

(define LOS1 (list "V" "6" "T" "1" "Z" "4"))

;; ==========
;; functions:

;; 1String -> Boolean
;; produce true if 1s is alphabetic/numeric

(check-expect (alphabetic? " ") false)
(check-expect (alphabetic? "1") false)
(check-expect (alphabetic? "a") true)
(check-expect (numeric? " ") false)
(check-expect (numeric? "1") true)
(check-expect (numeric? "a") false)

(define (alphabetic? 1s) (char-alphabetic? (string-ref 1s 0)))

(define (numeric? 1s) (char-numeric? (string-ref 1s 0)))

;; ListOf1String Pattern -> Boolean
;; to return true if the list matches the pattern
;; (define (pattern? l p) 0)

(check-expect (pattern? empty empty) true)
(check-expect (pattern? empty PATTERN1) true)
(check-expect (pattern? LOS1 empty) false)
(check-expect (pattern? LOS1 PATTERN1) true)
(check-expect (pattern? (list "1" "A" "2") PATTERN1) false)
(check-expect (pattern? (list "x" "3" "y") (list "A" "N" "A")) true)
(check-expect (pattern? (list "1" "3" "y") (list "A" "N" "A")) false)
(check-expect (pattern? (list "1" "a" "4") (list "N" "A" "N")) true)
(check-expect (pattern? (list "1" "b" "c") (list "N" "A" "N")) false)
(check-expect (pattern? (list "V" "6" "T" "1" "Z" "4") (list "A" "N" "A" "N" "A" "N")) true)

#;
(define (pattern? l p)
  (... (first l)
       (first p)
       (pattern? (rest l) ...)
       (pattern? (rest p) ...)))

(define (pattern? l p)
  (cond [(and (empty? l) (empty? p)) true]
        [(empty? l) true]
        [(empty? p) false]
        [else
         (if (string=? (first p) "A")
             (if (alphabetic? (first l)) (pattern? (rest l) (rest p)) false)
             (if (string=? (first p) "N")
                 (if (numeric? (first l)) (pattern? (rest l) (rest p)) false)
                 false))]))
