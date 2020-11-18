;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |odd from n|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Design a function called odd-from-n that consumes a natural number n, and produces a list of all the odd numbers from n down to 1.

;; Natural -> ListOfNatural
;; to return a list of all the odd numbers from n down to 1
;; (define (odd-from-n n) 0)

(check-expect (odd-from-n 0) empty)
(check-expect (odd-from-n 1) (cons 1 empty))
(check-expect (odd-from-n 5) (cons 5 (cons 3 (cons 1 empty))))
(check-expect (odd-from-n 8) (cons 7 (cons 5 (cons 3 (cons 1 empty)))))

#;
(define (fn-for-Natural n)
  (cond [(zero? n) (...)]
        [else
         (... n
              (fn-for-Natural (sub1 n)))]))

(define (odd-from-n n)
  (cond [(zero? n) empty]
        [else
         (if (odd? n) (cons n (odd-from-n (sub1 n)))
             (odd-from-n (sub1 n)))]))
