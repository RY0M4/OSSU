;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |evaluate foo|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Hand step the evaluation of (foo 3) given the definition of foo below.

(define (foo n)
  (local [(define x (* 3 n))]
    (if (even? x)
        n
        (+ n (foo (sub1 n))))))

#;
(define (foo 3)
  (define x_0 9)
  (if (even? 9) ; false
      3
      (+ 3 (foo 2))))

#;
(define (foo 2)
  (define x_0 6)
  (if (even? 6) ; true
      2
      (+ 2 (foo 1))))

#;
(+ 3 2)
5

(foo 3)