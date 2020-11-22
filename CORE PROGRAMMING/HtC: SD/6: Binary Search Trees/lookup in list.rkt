;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname lookup-in-list-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Design a function rhat searches for an account in a list.

(define-struct account (num name))
;; Accounts is one of:
;; - empty
;; - (cons (make-account Natural String) Accounts)
;; a list of accounts, where each 

(define ACS0 empty)
(define ACS1 (list (make-account 1 "abc") (make-account 4 "dcj") (make-account 3 "ilk") (make-account 7 "ruf")))

#;
(define (fn-for-accounts accs)
  (cond [(empty? accs) (...)]
        [else
         (... (account-num  (first accs)) 
              (account-name (first accs)) 
              (fn-for-accounts (rest accs)))]))
                                   
;; Accounts Natural -> String or false
;; Try to find account with given number in accounts. If found produce name, otherwise produce false.
;; (define (lookup accs n) "")

(check-expect (lookup ACS0 1) false)
(check-expect (lookup ACS1 1) "abc")
(check-expect (lookup ACS1 4) "dcj")
(check-expect (lookup ACS1 8) false)

;; template taken from the data definition

(define (lookup accs n)
  (cond [(empty? accs) false]
        [else
         (if (= n (account-num  (first accs)))
             (account-name (first accs))
             (lookup (rest accs) n))]))