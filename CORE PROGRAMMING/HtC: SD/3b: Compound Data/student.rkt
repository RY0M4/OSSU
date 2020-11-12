;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname student) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Design a data definition to help a teacher organize their next field trip. 
;; On the trip, lunch must be provided for all students.
;; For each student, track their name, their grade (from 1 to 12), and whether or not they have allergies.

(define-struct student (name grade allergic))
;; student is (make-student String Natural[1, 12] Boolean)
;; (make-student name grade allergic) is a student with
;; name is the name of the student
;; grade is the grade of the student
;; allergic determines whether or not the student has any allergies

(define student1 (make-student "Cloud" 9 false))
(define student2 (make-student "Auron" 12 false))
(define student3 (make-student "Samantha" 6 true))
(define student4 (make-student "Francesca" 3 true))

#;
(define (fn-for-student s)
  (... (student-name s)       ; String
       (student-grade s)      ; Natural[1, 12]
       (student-allergic s))) ; Boolean

;; template rules used:
;; - Compound: 3 fields

;; To plan for the field trip, if students are in grade 6 or below, the teacher is responsible for keeping track of their allergies.
;; If a student has allergies, and is in a qualifying grade, their name should be added to a special list. 
;; Design a function to produce true if a student name should be added to this list.

;; student -> Boolean
;; to return true if the student has any allergies
;; (define (check-allergic student) 0)

(check-expect (check-allergic student1) false)
(check-expect (check-allergic student2) false)
(check-expect (check-allergic student3) true)
(check-expect (check-allergic student4) true)

;; template taken from the data definition

(define (check-allergic student)
  (and (<= (student-grade student) 6) (student-allergic student)))

(check-allergic student1)
(check-allergic student2)
(check-allergic student3)
(check-allergic student4)