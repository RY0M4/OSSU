;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname sum-keys-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Design a function that consumes a BST and produces the sum of all the keys in the BST.

;; data definitions:

(define-struct node (key val l r))
;; a BST (Binary Search Tree) is one of:
;; - false
;; - (make-node Integer String BST BST)
;; false means no BST, or empty BST
;; key is the node key
;; val is the node val
;; l and r are left and right subtrees
;; INVARIANT: for a given node:
;; key is > all keys in its l(eft)  child
;; key is < all keys in its r(ight) child
;; the same key never appears twice in the tree

(define BST0 false)
(define BST1 (make-node 1 "abc" false false))
(define BST4 (make-node 4 "dcj" false (make-node 7 "ruf" false false)))
(define BST3 (make-node 3 "ilk" BST1 BST4))
(define BST42 (make-node 42 "ily" (make-node 27 "wit" (make-node 14 "olp" false false) false) false))
(define BST10 (make-node 10 "why" BST3 BST42))

#;
(define (fn-for-bst t)
  (cond [(false? t) (...)]
        [else
         (... (node-key t)
              (node-val t)    
              (fn-for-bst (node-l t))
              (fn-for-bst (node-r t)))]))

;; template rules used:
;; - one of: 2 cases
;; - atomic distinct: false
;; - compound: (make-node Integer String BST BST)
;; - self reference: (node-l t) has type BST
;; - self reference: (node-r t) has type BST

;; BST -> Natural
;; to produce the sum of all the keys in the BST
;; (define (sum-keys BST) 0)

(check-expect (sum-keys BST0) 0)
(check-expect (sum-keys BST1) 1)
(check-expect (sum-keys BST4) 11)
(check-expect (sum-keys BST3) 15)
(check-expect (sum-keys BST42) 83)
(check-expect (sum-keys BST10) 108)

;; template taken from the data definition

(define (sum-keys bst)
  (cond [(false? bst) 0]
        [else
         (+ (node-key bst)
            (sum-keys (node-l bst))
            (sum-keys (node-r bst)))]))


