;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname movie) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Design a data definition to represent a movie, including title, budget, and year released.

(define-struct movie (name cost date))
;; movie is (make-movie String Natural Natural)
;; (make-movie name money date) is a movie with
;; name is the name of the movie
;; cost is the cost of the movie
;; date is the release date of the movie

(define movie1 (make-movie "Titanic" 200000000 1997))
(define movie2 (make-movie "Avatar" 237000000 2009))       
(define movie3 (make-movie "The Avengers" 220000000 2012))

#;
(define (fn-for-movie m)
  (... (movie-name m)   ; String
       (movie-cost m)   ; Natural
       (movie-date m))) ; Natural

;; template rules used:
;; - Compound: 3 fields

;; Design a function that consumes two movies and produces the title of the most recently released movie.

;; movie movie -> String
;; to return the most recently released movie (between two movies)
;; (define (chrono-movie m1 m2) 0)

(check-expect (chrono-movie movie1 movie2) "Avatar")
(check-expect (chrono-movie movie2 movie3) "The Avengers")
(check-expect (chrono-movie movie3 movie1) "The Avengers")

#;
(define (fn-for-movie m1 m2)
  (... (movie-name m1)
       (movie-name m2)
       (movie-cost m1)
       (movie-cost m2)
       (movie-date m1)
       (movie-date m2)))

(define (chrono-movie m1 m2)
  (if (> (movie-date m1) (movie-date m2)) (movie-name m1) (movie-name m2)))

(chrono-movie movie1 movie2)
(chrono-movie movie2 movie3)
(chrono-movie movie3 movie1)
