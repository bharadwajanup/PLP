#lang racket
;(require "a1-student-tests.rkt")
;(test-file #:file-name "a1.rkt")
(print-as-expression #f)

;1. 
(define countdown 
  (lambda (n)
    (cond [(zero? n) '(0)]
          [(cons n (countdown (sub1 n)))])))

;2
(define insertR 
  (lambda (x y lst)
    (cond
      [(null? lst) '()]
      [(eqv? x (car lst)) (cons x (cons y (insertR x y (cdr lst))))]
      [else (cons (car lst) (insertR x y (cdr lst)))])))

;3.
(define remv-1st 
  (lambda (x lst)
    (cond
      [(null? lst) '()]
      [(eqv? x (car lst)) (remv-1st null (cdr lst)) ]
      [else (cons (car lst) (remv-1st x (cdr lst)))])))

;4
(define count-?s
  (lambda (lst)
    (cond
      [(null? lst) 0]
      [(eqv? '? (car lst)) (add1 (count-?s (cdr lst)))]
      [else (count-?s (cdr lst))])))

;5
(define filter
  (lambda (pred lst)
    (cond
      [(null? lst) '()]
      [ (pred (car lst)) (cons (car lst) (filter pred (cdr lst))) ]
      [else (filter pred (cdr lst)) ])))


;6
(define zip
  (lambda (a b)
    (cond
      [(null? a) '()]
      [(null? b) '()]
      [else (cons (cons (car a) (car b)) (zip (cdr a) (cdr b))) ])))

;7
(define map
  (lambda (proc lst)
    (cond
      [(null? lst) '()]
      [else (cons (proc (car lst)) (map proc (cdr lst)))])))

;8
(define append
  (lambda (a b)
    (cond
      [(null? a) b]
      [else (cons (car a) (append (cdr a) b))])))


;9
(define reverse
  (lambda (lst)
    (cond
      [(null? lst) '()]
      [else (append (reverse (cdr lst))  (list (car lst)))])))

;10
(define fact
  (lambda (n)
    (cond
      [(zero? n) 1]
      [else (* n (fact (sub1 n)))])))

;11
(define member-?*
  (lambda (lst)
    (cond
      [(null? lst) #f]
      [(pair? (car lst)) (member-?* (car lst))]
      [(eqv? '? (car lst)) #t]
      [else (member-?* (cdr lst))])))

;12
(define fib
  (lambda (n)
    (cond
      [(zero? n) 0]
      [(eqv? 1 n) 1]
      [else (+ (fib (sub1 n)) (fib (- n 2)))])))

;13 '((w x) y (z)) '((w . (x . ())) . (y . ((z . ())))))

;;(equal? '((w x) y (z)) '((w . (x . ())) . (y . ((z . ())))))


;14
(define binary->natural
  (lambda(lst)
    (cond
      [(null? lst) 0]
      [else (+ (car lst) (* (binary->natural (cdr lst)) 2))])))

(define binary->natural-fr
  (lambda (lst)
    (foldr (lambda (a b)
             (+ a (* b 2))) 0 lst)))
;15
(define minus
  (lambda (a b)
    (cond
      [(zero? b) a]
      [(minus (sub1 b) a)])))

;16
(define div
  (lambda (a b)
    (cond
      [(zero? a) 0]
      [(eq? a b) 1]
      [(add1 (div (minus a b) b ))])))

;17
(define append-map
  (lambda (proc lst)
    (cond
      [(null? lst) '()]
      [else (append (proc (car lst)) (append-map proc (cdr lst)))])))

(define append-map-fr
  (lambda (proc lst)
    (foldr (lambda (a b)
             (append-fr (proc a) b)) '() lst)))

;18
(define present?
  (lambda (a lst)
    (cond
      [(null? lst) #f]
      [(eqv? a (car lst)) #t]
      [else (present? a (cdr lst))])))

(define set-difference
  (lambda (a b)
    (cond
      [(null? a) '()]
      [(present? (car a) b) (set-difference (cdr a) b)]
      [else (cons (car a) (set-difference (cdr a) b))])))

;19 Power set

;;Solution could be obtained by concatenating nCl+nC(l-1)+nC(l-2)+..nC0 where l is the length of the list. Was not able to put it as a code.

;20 Cartesian Product

(define get-tuples
  (lambda (a lst)
    (cond
      [(null? lst) '()]
      [else (cons (cons a (list (car lst))) (get-tuples a (cdr lst)))])))

(define cartesian-product
  (lambda (lst)
    (cond
      [(null? (car lst)) '()]
      [else (append (get-tuples (car (car lst)) (car (cdr lst))) (cartesian-product (cons (cdr (car lst)) (cdr lst))))])))

(define cartesian-prod-helper
  (lambda (lst1 lst2)
    (cond 
    [(null? lst1) '()]
    [(null? lst2) '()]
    [else (append (get-tuples (car lst1) lst2) (cartesian-prod-helper (cdr lst1) lst2))])))


(define cartesian-product-fr
  (lambda (lst)
    (foldr (lambda (a b)
             (cond
               [(null? b) (car (cdr lst))]
             [else (cartesian-prod-helper a b)])) '() lst)))


;21 Rewrite some of the natural recursive functions using foldr

(define insertR-fr
  (lambda (x y lst)
    (foldr (lambda (a b)
             (cond
               [(eqv? a x) (cons a (cons y b))]
               [else (cons a b)])) '() lst)))

(define count-?s-fr
  (lambda (lst)
    (foldr (lambda (a b)
             (cond
               [(eqv? a '?) (add1 b)]
               [b])) 0 lst)))

(define map-fr
  (lambda (proc lst)
    (foldr (lambda (a b)
             (cons (proc a) b)) '() lst)))

(define append-fr
  (lambda (a b)
    (foldr (lambda (x y)
             (cons x y)) b a)))

(define reverse-fr
  (lambda (lst)
    (foldr (lambda (a b)
             (append-fr b (list a))) '() lst)))

(define filter-fr
  (lambda (pred lst)
    (foldr (lambda (a b)
             (cond
               [(pred a) (cons a b)]
               [else b])) '() lst)))

(define set-difference-fr
  (lambda (a b)
    (foldr (lambda (x y)
             (cond
               [(present? x b) y]
               [else (cons x y)])) '() a)))

;22
(define collatz
   (letrec
       ((odd-case
         (lambda (recur)
           (lambda (x)
             (cond 
               ((and (positive? x) (odd? x)) (collatz (add1 (* x 3)))) 
               (else (recur x))))))
        (even-case
         (lambda (recur)
           (lambda (x)
             (cond 
               ((and (positive? x) (even? x)) (collatz (/ x 2))) 
               (else (recur x))))))
        (one-case
         (lambda (recur)
           (lambda (x)
             (cond
               ((zero? (sub1 x)) 1)
               (else (recur x))))))
        (base
         (lambda (x)
           (error 'error "Invalid value ~s~n" x))))
    (one-case (odd-case (even-case base));; this should be a single line, without lambda
     )))