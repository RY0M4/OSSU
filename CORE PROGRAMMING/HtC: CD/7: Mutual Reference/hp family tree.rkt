;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |hp family tree|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; In this problem set you will represent information about descendant family trees from Harry Potter and design functions that operate on those trees.
;; To make your task much easier we suggest two things:
;; - You only need a DESCENDANT family tree.
;; - Read through this entire problem set carefully to see what information.
;; - The functions below are going to need. Design your data definitions to only represent that information.
;; - You can find all the information you need by looking at the individual character pages like the one we point you to for Arthur Weasley.

;; Design a data definition that represents a family tree from the Harry Potter wiki, which contains all necessary information for the other problems.
;; You will use this data definition throughout the rest of the homework.

(define-struct wizard (name surname wand patronus family))
;; wizard is (make-wizard name surname wand patronus family)
;; a magical person from hp
;; name is their name
;; surname is their (family) surname
;; wand is their wand (the stick you use to cast spells)
;; patronus is their patronus (a weird animal spirit)
;; family is their list of relatives

#;
(define (fn-for-wizard m)
  (... (wizard-name m)
       (wizard-surname m)
       (wizard-wand m)
       (wizard-patronus m)
       (fn-for-family (wizard-family m))))

;; family is one of:
;; - empty
;; - (cons wizard family)
;; a family is made up of multiple wizards with the same surname

#;
(define (fn-for-family f)
  (cond [(empty? f) ...]
        [else
         (... (fn-for-wizard (first f))
              (fn-for-family (rest f)))]))

;; Define a constant named ARTHUR that represents the descendant family tree for Arthur Weasley.
;; Note that some of the information may be missing.
;; Enter that information with a special value of "" (the empty string) meaning it is not present.

(define ARTHUR (cons (make-wizard "Arthur" "Weasley" "ash" "weasel"
                                  (cons (make-wizard "Fred" "Weasley" "oak" "magpie" (cons (make-wizard "George" "Weasley" "oak" "magpie"
                                                                                                        (cons (make-wizard "Ronald" "Weasley" "ash" "terrier" (cons (make-wizard "Ginevra" "Weasley" "spruce" "horse"
                                                                                                                                                                                 (cons (make-wizard "Rose" "Weasley" "" "" (cons (make-wizard "Hugo" "Weasley" "" ""
                                                                                                                                                                                                                                              (cons (make-wizard "James" "Potter" "" "" (cons (make-wizard "Albus" "Potter" "" ""
                                                                                                                                                                                                                                                                                                           (cons (make-wizard "Lily" "Potter" "spruce" "" empty) empty)) empty)) empty)) empty)) empty)) empty)) empty)) empty)) empty)) empty))

;; Design a function that produces a pair list (i.e. list of two-element lists) of every person in the tree and his or her patronus.

;; family -> ListOfString
;; to return pair list of every person in the family and his or her patronus
;; (define (patronus-f f) 0)

;; template taken from the data definition

(define (patronus-f f)
  (cond [(empty? f) empty]
        [else
         (append (patronus-m (first f))
                 (patronus-f (rest f)))]))

;; wizard -> String
;; to return the patronus of a wizard
;; (define (patronus-m m) 0)

;; template taken from the data definition

(define (patronus-m m)
  (cons (list (wizard-name m) (wizard-surname m) (wizard-patronus m)) (patronus-f (wizard-family m))))

(patronus-f ARTHUR)

;; Design a function that produces the names of every person in a given tree whose wands are made of a given material. 

(define MATERIAL "ash")

;; family -> ListOfString
;; to return a list of people (from a family) with wands made of the same material
;; (define (wand-sort f) 0)

;; template taken from the data definition

(define (wand-sort-f f)
  (cond [(empty? f) empty]
        [else
         (append (wand-sort-m (first f))
                 (wand-sort-f (rest f)))]))

;; wizard -> Boolean
;; to return true if the wand of a specific wizard is of a certain material
;; (define (wand-sort-m m) 0)

;; template taken from the data definition

(define (wand-sort-m m)
  (if (string=? (wizard-wand m) MATERIAL)
      (cons (wizard-name m) (wand-sort-f (wizard-family m)))
      (wand-sort-f (wizard-family m))))

(wand-sort-f ARTHUR)
       

