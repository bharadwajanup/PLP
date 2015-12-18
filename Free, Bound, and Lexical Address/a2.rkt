#lang racket
(print-as-expression #f)

;;1

(define list-ref
  (lambda (ls n)
    (letrec
        ((nth-cdr
         (lambda (n)
	   (cond
             [(zero?  n)  ls]
             [else (cdr (nth-cdr (sub1 n)))]     
              ))))
      (car (nth-cdr n)))))

;;2

(define union
  (lambda (ls1 ls2)
    (cond
      [(null? ls1) ls2]
      [(memv (car ls1) ls2) (union (cdr ls1) ls2)]
      [else (cons (car ls1) (union (cdr ls1) ls2))])))

;;3

(define extend
  (lambda (n pred)
     (lambda (x)
        (cond
          [(eqv? n x) #t]
          [(pred x) #t]
          [else #f]))
      ))

;;4
(define walk-symbol
  (lambda (x lst)
    (cond
      [(null? lst) x]
      [(eqv? x  (car (car (reverse lst)))) (walk-symbol  (cdr (car (reverse lst))) (reverse (cdr (reverse lst))))]
      [else (walk-symbol x  (cdr (reverse lst)))]
      )))

;;1

(define lambda->lumbda
  (lambda (exp)
    (match exp
      [`,y #:when (symbol? y)  y]
      [`(lambda (,x) ,body) (cons 'lumbda (cons (list x) `(,(lambda->lumbda body))))]
      [`(,rator ,rand) `(,(lambda->lumbda rator) ,(lambda->lumbda rand))])))

;6

(define var-occurs?
  (lambda (var exp)
    (match exp
      [`,x #:when (eqv? var x) #t]
      [`,x #:when (symbol? x) #f]
      [`(lambda (,x) ,body) (var-occurs? var body)]
      [ `(,rator ,rand) (if (var-occurs? var rator) #t (var-occurs? var rand))])))

;7
(define vars
  (lambda (exp)
    (match exp
      [`,y #:when (symbol? y) (list y)]
      [`(lambda (,x) ,body) (vars body)]
      [`(,rator ,rand) (append (vars rator) (vars rand))])))

;8

(define unique-vars 
  (lambda (exp)
    (match exp
      [`,y #:when (symbol? y) (list y)]
      [`(,y) #:when (symbol? y) (list y)]
      [`(lambda (,x) ,body) (unique-vars body)]
      [`(,rator ,rand) (union (unique-vars rator) (unique-vars rand))])))

;9

(define var-occurs-free?
  (lambda (var exp)
    (match exp
    [`,y #:when (eq? var y) #t ]
      [`,y #:when (symbol? y) #f]
    [`(lambda (,x) ,body) (if (var-occurs? var body) (if (eq? var x) #f #t) #f)]
    [`(,rator ,rand)   (if (var-occurs-free? var rator) #t (var-occurs-free? var rand)) ])))

;10
(define var-occurs-bound? 
  (lambda (var exp)
    (match exp
      [`,y #:when (symbol? y) #f]
      [ `(lambda (,x) ,body) (if (var-occurs? var body) (if (eq? var x) #t (var-occurs-bound? var body)) #f)]
      [`(,rator ,rand)   (if (var-occurs-bound? var rator) #t (var-occurs-bound? var rand)) ])))

;11

(define unique-free-vars
  (lambda (exp)
    (match exp
      [`,y #:when (symbol? y) `(,y)]
      [`(lambda (,x) ,body)  (remove x (unique-free-vars body))]
      [`(,rator ,rand) (union  (unique-free-vars rator)  (unique-free-vars rand))])))

;12

(define unique-bound-vars
  (lambda (exp)
    (match exp
      [`,y #:when (symbol? y) `()]
      [`(lambda (,x) ,body) (if (var-occurs? x body) (cons x (unique-bound-vars body)) (unique-bound-vars body))]
      [`(,rator ,rand) (union  (unique-bound-vars rator)  (unique-bound-vars rand))])))

;13
(define acc-gen
  (lambda (y lst)
    (cond
      [(null? lst) (list (cons y '(0)))]
      [(eq? (caar lst) y)  (acc-gen y (cdr lst))]
      [else (cons (cons (caar lst) (list (add1 (car (cdr (car lst)))))) (acc-gen y (cdr lst)))])))

(define lex
  (lambda (exp acc)
    (match exp
      [`,x #:when (symbol? x) (cons 'var (list (car (cdr (assoc x acc)))))]
      [`(lambda (,x) ,body) (cons 'lambda (list (lex body (acc-gen x acc))))]
      [`(,rotor ,rand) `(,(lex rotor acc) ,(lex rand acc))])))

;14