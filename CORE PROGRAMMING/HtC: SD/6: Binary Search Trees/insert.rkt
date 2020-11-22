;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname insert-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Design a function that consumes an Integer, String and BST, and adds a node that has the given key and value to the tree.
;; The node should be inserted in the proper place in the tree.
;; The function can assume there is not already an entry for that number in the tree.
;; The function should produce the new BST.

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

;; Integer String BST -> BST
;; to add a node to the BST in the proper place
;; (define (insert-node k v t) 0)

(check-expect (insert-node 1 "qja" BST0) (make-node 1 "qja" false false))
(check-expect (insert-node 5 "jwl" BST1) (make-node 1 "abc" false (make-node 5 "jwl" false false)))
(check-expect (insert-node 6 "gbo" BST4) (make-node 4 "dcj" false (make-node 7 "ruf" (make-node 6 "gbo" false false) false)))
(check-expect (insert-node 8 "sxh" BST42) (make-node 42 "ily" (make-node 27 "wit" (make-node 14 "olp" (make-node 8 "sxh" false false) false) false) false))

;; template taken from the data definition

(define (insert-node k v t)
  (cond [(false? t) (make-node k v false false)]
        [else
         (if (< k (node-key t))
             (make-node (node-key t) (node-val t) (insert-node k v (node-l t)) (node-r t))
             (make-node (node-key t) (node-val t) (node-l t) (insert-node k v (node-r t))))]))