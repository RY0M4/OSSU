;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname local-design-quiz) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Suppose you have rosters for players on two opposing tennis team, and each roster is ordered by team rank, with the best player listed first.
;; When both teams play, the best players of each team play one another, and the second-best players play one another, and so on down the line.
;; When one team has more players than the other, the lowest ranking players on the larger team do not play.
;; Design a function that consumes two rosters, and produces true if all players on both teams will play if the teams play each other. 

;; Player is String
;; the name of a tennis player

(define P1 "Maria")
(define P2 "Serena")

#;
(define (fn-for-player p)
  (... p))

;; template rules used:
;; - atomic non-distinct: String

;; Roster is one of:
;; - empty
;; - (cons Player Roster)
;; a team roster, ordered from best player to worst

(define R0 empty)
(define R1 (list "Eugenie" "Gabriela" "Sharon" "Aleksandra"))
(define R2 (list "Maria" "Nadia" "Elena" "Anastasia" "Svetlana"))

#;
(define (fn-for-roster r)
  (cond [(empty? r) (...)]
        [else 
         (... (fn-for-player (first r))
              (fn-for-roster (rest r)))]))

;; template rules used:
;; one of: 2 cases
;; atomic distinct: empty
;; compound: 2 fields
;; reference: (first r) is Player
;; self-reference: (rest r) is Roster

;; Roster Roster -> Boolean
;; to return true if all players on both teams will play if the teams play each other
;; (define (will-all-play? r1 r2) 0)

(check-expect (will-all-play? R0 R0) true)
(check-expect (will-all-play? R0 R1) false)
(check-expect (will-all-play? R1 R2) false)
(check-expect (will-all-play? (list "Barbara" "Samantha" "Margherita" "Francesca") R1) true)

;; template taken from the data definition

(define (will-all-play? r1 r2)
  (cond [(and (empty? r1) (empty? r2)) true]
        [(or (empty? r1) (empty? r2)) false]
        [else
         (if (is-string? (first r1) (first r2))
             (will-all-play? (rest r1) (rest r2))
             false)]))

;; Player Player -> Boolean
;; to return true if the player is a string
;; (define (is-string? p1 p2) 0)

(check-expect (is-string? P1 P2) true)
(check-expect (is-string? 1 2) false)
(check-expect (is-string? P1 2) false)

;; template taken from the data definition

(define (is-string? p1 p2)
  (and (string? p1) (string? p2)))
        
;; Now write a function that, given two teams, produces the list of tennis matches that will be played.
;; You can assume the two teams have the same number of players. 

(define-struct match (p1 p2))
;; Match is (make-match Player Player)
;; a match between player p1 and player p2, with same team rank

(define M0 (make-match "Eugenie" "Maria"))
(define M1 (make-match "Gabriela" "Nadia"))

#;
(define (fn-for-match m)
  (... (match-p1 m) (match-p2 m)))

;; template rules used:
;; - compound: 2 fields

;; ListOfMatch is one of:
;; - empty
;; - (cons Match ListOfMatch)
;; a list of matches between one team and another.

(define LOM0 empty)
(define LOM1 (list (make-match "Eugenie" "Maria") (make-match "Gabriela" "Nadia")))

#;
(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]
        [else
         (... (fn-for-match (first lom))
              (fn-for-lom (rest lom)))]))

;; template rules used:
;; one of: 2 cases
;; atomic distinct: empty
;; compound: 2 fields
;; reference: (first lom) is Match
;; self-reference: (rest lom) is ListOfMatch

;; Roster Roster -> ListOfMatch
;; to return the list of tennis matches that will be played.
;; (define (game r1 r2) 0)

(check-expect (game R0 R1) empty)
(check-expect (game R1 (list "Maria" "Nadia" "Elena" "Anastasia")) (list (make-match "Eugenie" "Maria") (make-match "Gabriela" "Nadia") (make-match "Sharon" "Elena") (make-match "Aleksandra" "Anastasia"))) 

;; template taken from the data definition

(define (game r1 r2)
  (cond [(or (empty? r1) (empty? r2)) empty]
        [else 
         (cons (make-match (first r1) (first r2))
               (game (rest r1) (rest r2)))]))


         
         
             









