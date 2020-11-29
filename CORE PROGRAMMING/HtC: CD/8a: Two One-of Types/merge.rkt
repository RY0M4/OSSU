;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname merge) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Design the function merge.
;; It consumes two lists of numbers, which it assumes are each sorted in ascending order.
;; It produces a single list of all the numbers, also sorted in ascending order. 
;; Your solution should explicitly show the cross product of type comments table, filled in with the values in each case.
;; Your final function should have a cond with 3 cases.
;; You can do this simplification using the cross product table by recognizing that there are subtly equal answers.

;; ListOfNumber ListOfNumber -> ListOfNumber
;; to return a merge of two lists in ascending order
;; (define (merge lon1 lon2) 0)

(check-expect (merge empty empty) empty)
(check-expect (merge empty (list 1 2 3)) (list 1 2 3))
(check-expect (merge (list 1 2 3) empty) (list 1 2 3))
(check-expect (merge (list 1 2 3) (list 1 2 3)) (list 1 1 2 2 3 3))
(check-expect (merge (list 2 4 6) (list 1 3 5)) (list 1 2 3 4 5 6))
(check-expect (merge (list 1 3 5) (list 2 4 6)) (list 1 2 3 4 5 6))

#;
(define (merge lon1 lon2)
  (... (first lon1)
       (first lon2)
       (merge (rest lon1))
       (merge (rest lon2))))

(define (merge lon1 lon2)
  (cond [(and (empty? lon1) (empty? lon2)) empty]
        [(empty? lon1) lon2]
        [(empty? lon2) lon1]
        [else
         (if (<= (first lon1) (first lon2))
             (cons (first lon1) (merge (rest lon1) lon2))
             (cons (first lon2) (merge lon1 (rest lon2))))]))
            

        

