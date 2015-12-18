#lang racket
(require racket/trace)

(define empty-k
  (lambda ()
    (let ((once-only #f))
      (lambda (v)
        (if once-only
	    (error 'empty-k "You can only invoke the empty continuation once")
	    (begin (set! once-only #t) v))))))


;1

(define binary-to-decimal
  (lambda (n)
    (cond
      [(null? n) 0]
      [else (+ (car n) (* 2 (binary-to-decimal (cdr n))))])))

(define binary-to-decimal-cps
  (lambda(n k)
    (cond
      [(null? n) (k 0)]
      [else (binary-to-decimal-cps (cdr n) (lambda(x) (k (+ (car n) (* 2 x)))))])))

;;(binary-to-decimal-cps '(1 1 0 1) (empty-k))
;;(binary-to-decimal-cps '(0 1) (empty-k))
;;(binary-to-decimal-cps '(1) (empty-k))
;;(binary-to-decimal-cps '() (empty-k))


;2

(define times
  (lambda (ls)
    (cond
      [(null? ls) 1]
      [(zero? (car ls)) 0]
      [else (* (car ls) (times (cdr ls)))])))


(define times-cps
  (lambda (ls k)
    (cond
      [(null? ls) (k 1)]
      [(zero? (car ls)) (k 0)]
      [else (times-cps (cdr ls) (lambda(x) (k (* (car ls) x))))])))

;;(times-cps '(1 2 3 1 3 1 2 3 5 6 7 7 7 1) (empty-k))
;;(times-cps '(1 2 3 1 3) (empty-k))
;;(times-cps '(1 2 3 0 3) (empty-k))
;;(times-cps '(1 2 3 4 5) (empty-k))

;3

(define times-cps-shortcut
  (lambda (ls k)
    (cond
      [(null? ls) (k 1)]
      [(zero? (car ls)) 0]
      [else (times-cps-shortcut (cdr ls) (lambda(x) (k (* (car ls) x))))])))

;;(times-cps-shortcut '(1 2 3 1 3) (empty-k))

;4

(define plus
  (lambda (m)
    (lambda (n)
      (+ m n))))

(define plus-cps
  (lambda (m)
    (lambda (n k)
      (k (+ m n)))))

;; ((plus-cps ((plus-cps 2) 3 (empty-k))) 5 (empty-k))
;; ((plus-cps 2) 3 (empty-k))

;5


(define remv-first-9*
  (lambda (ls)
    (cond
      [(null? ls) '()]
      [(pair? (car ls))
       (cond
         [(equal? (car ls) (remv-first-9* (car ls))) (cons (car ls) (remv-first-9* (cdr ls)))]
         [else (cons (remv-first-9* (car ls)) (cdr ls))])]
      [(eqv? (car ls) '9) (cdr ls)]
      [else (cons (car ls) (remv-first-9* (cdr ls)))])))


(define remv-first-9*-cps
  (lambda (ls k)
    (cond
      [(null? ls) (k '())]
      [(pair? (car ls)) (remv-first-9*-cps (car ls) (lambda(x^)
                                                       (cond
                                                        [(equal? (car ls) x^)  (remv-first-9*-cps (cdr ls) (lambda(x^^) (k (cons (car ls) x^^))))]
                                                       
                                                         [else (remv-first-9*-cps (car ls) (lambda(x^^^) (k (cons x^^^ (cdr ls)))))])))]
      
      [(eqv? (car ls) '9) (k (cdr ls))]
      [else (remv-first-9*-cps (cdr ls) (lambda(x) (k (cons (car ls) x))))])))




#|
   (remv-first-9*-cps '((1 2 (3) 9)) (empty-k))
   (remv-first-9*-cps '(9 (9 (9 (9)))) (empty-k)) |#


;6

(define count-syms*
  (lambda (ls)
    (cond
      [(null? ls) 0]
      [(pair? (car ls)) (+ (count-syms* (car ls)) (count-syms* (cdr ls)))]
      [(symbol? (car ls)) (add1 (count-syms* (cdr ls)))]
      [else (count-syms* (cdr ls))])))


(define count-syms*-cps
  (lambda (ls k)
    (cond
      [(null? ls) (k 0)]
      [(pair? (car ls))  (count-syms*-cps (car ls) (lambda(x) (k (count-syms*-cps (cdr ls) (lambda(x^) (+ x x^))))))]
      [(symbol? (car ls)) (count-syms*-cps (cdr ls) (lambda(x) (k (add1 x))))]
      [else (count-syms*-cps (cdr ls) k)])))



#|
(count-syms*-cps '(a 1 b 2 c 3) (empty-k))
(count-syms*-cps '((a 1) (b 2) (c 3)) (empty-k))
(count-syms*-cps '(1 (b (3 (d (5 e) 7) (g)) 9) ((h))) (empty-k))
|#


;7

(define cons-cell-count
  (lambda (ls)
    (cond
      [(pair? ls)
       (add1 (+ (cons-cell-count (car ls)) (cons-cell-count (cdr ls))))]
      [else 0])))

(define cons-cell-count-cps
  (lambda (ls k)
    (cond
      [(pair? ls)
       (cons-cell-count-cps (car ls) (lambda(x^) (k (add1 (cons-cell-count-cps (cdr ls) (lambda(x^^) (+ x^ x^^)))))))]
      [else (k 0)])))

#|
(cons-cell-count-cps '(((((0)))) 1 2 3 4 5) (empty-k))
(cons-cell-count-cps '(((((0))))) (empty-k))
(cons-cell-count-cps (cons (cons 2 3) (cons (cons 5 6) (cons '() '()))) (empty-k))

|#


;8


(define find 
  (lambda (u s)
    (let ((pr (assv u s)))
      (if pr (find (cdr pr) s) u))))

(define find-cps 
  (lambda (u s k)
    (let ((pr  (assv u s)))
      (if pr (find-cps (cdr pr) s k) u))))

#|
(find-cps 5 '((5 . a) (6 . b) (7 . c)) (empty-k))
(find-cps 7 '((5 . a) (6 . 5) (7 . 6)) (empty-k))
(find 5 '((5 . 6) (9 . 6) (2 . 9)))
|#

;9

;; ack: computes the Ackermann function
;; (http://en.wikipedia.org/wiki/Ackermann_function).  Warning: if you
;; run this program with m >= 4 and n >= 2, you'll be in for a long
;; wait.
(define ack
  (lambda (m n)
    (cond
      [(zero? m) (add1 n)]
      [(zero? n) (ack (sub1 m) 1)]
      [else (ack (sub1 m)
                 (ack m (sub1 n)))])))


;; ack: computes the Ackermann function
;; (http://en.wikipedia.org/wiki/Ackermann_function).  Warning: if you
;; run this program with m >= 4 and n >= 2, you'll be in for a long
;; wait.
(define ack-aps
  (lambda (m n k)
    (cond
      [(zero? m) (k (add1 n))]
      [(zero? n) (ack-aps (sub1 m) 1 k)]
      [else (ack-aps m (sub1 n) (lambda(x) (ack-aps (sub1 m)
                 x k)))])))

#|
(ack-aps 3 2 (empty-k))
(ack-aps 1 2 (empty-k))
|#

;10

(define fib
  (lambda (n)
    ((lambda (fib)
       (fib fib n))
     (lambda (fib n)
       (cond
	 [(zero? n) 0]
	 [(= 1 n) 1]
	 [else (+ (fib fib (sub1 n)) (fib fib (sub1 (sub1 n))))])))))

(define fib-cps
  (lambda (n k)
    ((lambda (fib-cps k^)
       (fib-cps fib-cps n k^))
     (lambda (fib-cps n k^^)
       (cond
	 [(zero? n) (k^^ 0)]
	 [(= 1 n) (k^^ 1)]
	 [else (fib-cps fib-cps (sub1 n) (lambda(x^) (k^^ (fib-cps fib-cps (sub1 (sub1 n)) (lambda(x^^) (+ x^ x^^))))))])) k)))


#|

(fib-cps 6 (empty-k))
(fib-cps 21 (empty-k))

|#



;11


(define unfold
  (lambda (p f g seed)
    ((lambda (h)
       ((h h) seed '()))
     (lambda (h)
       (lambda (seed ans)
	 (if (p seed)
	     ans
	     ((h h) (g seed) (cons (f seed) ans))))))))

(define null?-cps
    (lambda (ls k)
      (k (null? ls))))
(define car-cps
    (lambda (pr k)
      (k (car pr))))
 (define cdr-cps
    (lambda (pr k)
      (k (cdr pr))))

(define unfold-cps
  (lambda (p f g seed k)
    ((lambda (h k^)
       (h h (lambda(x^) (x^ seed '() k^))))
     (lambda (h k^^)
       (k^^ (lambda (seed ans k^^^)
	 (p seed (lambda(x^^) (if x^^
	     (k^^^ ans)
	     (h h (lambda(x^^^) (g seed (lambda(x^^^^) (f seed (lambda(x^^^^^) (x^^^ x^^^^ (cons x^^^^^ ans) k^^^))))))))))))) k)))






#|

(unfold-cps null?-cps car-cps cdr-cps '(a b c d e) (empty-k))

|#


;12

(define empty-s
  (lambda ()
    '()))

(define unify
  (lambda (u v s)
    (cond
      ((eqv? u v) s)
      ((number? u) `((,u . ,v) . ,s))
      ((number? v) (unify v u s))
      ((pair? u)
       (if (pair? v)
	   (let ((s (unify (find (car u) s) (find (car v) s) s)))
             (if s (unify (find (cdr u) s) (find (cdr v) s) s) #f))
	   #f))
      (else #f))))
 
(define unify-cps
  (lambda (u v s k)
    (cond
      ((eqv? u v) (k s))
      ((number? u) (k `((,u . ,v) . ,s)))
      ((number? v) (unify-cps v u s k))
      ((pair? u)
       (if (pair? v)
	   (unify-cps (find-cps (car u) s (empty-k)) (find-cps (car v) s (empty-k)) s (lambda(v^)
                                                                                        (let ((s v^))
                                                                                          (k (unify-cps (find-cps (cdr u) s (empty-k)) (find-cps (cdr v) s (empty-k)) s (lambda(v^^)
                                                                                                                                                                          (if s v^^ #f)))))))
	   (k #f)))
      (else (k #f)))))



#|

(unify-cps '(1 2 3) '(x 1 2) (empty-s) (empty-k))
(unify-cps 'x 'y (empty-s) (empty-k))
(unify-cps '(x x) '(5 6) (empty-s) (empty-k))
(unify-cps 'x 5 (unify-cps 'x 6 (empty-s) (empty-k)) (empty-k))
(unify-cps '(x y) '(5 6) (empty-s) (empty-k))
(unify-cps 'x 5 (unify-cps 'y 6 (empty-s) (empty-k)) (empty-k))
(unify-cps 'x 5 (empty-s) (empty-k))
|#



;13
(define add1-cps
  (lambda(x k)
    (k (add1 x))))

(define M
  (lambda (f)
    (lambda (ls)
      (cond
        ((null? ls) '())
        (else (cons (f (car ls)) ((M f) (cdr ls))))))))

(define M-cps
  (lambda (f k)
     (k (lambda (ls k^)
      (cond
        ((null? ls) (k^ '()))
        (else (f (car ls) (lambda(x^) (M-cps f (lambda(x^^) (x^^ (cdr ls) (lambda(x^^^) (k^ (cons x^ x^^^))))))))))))))

#|

((M-cps add1-cps (empty-k)) '(1 2 3) (empty-k))
|#

;14

(define use-of-M
  ((M (lambda (n) (add1 n))) '(1 2 3 4 5)))


(define use-of-M-cps
  ((M-cps (lambda (n k) (add1-cps n k)) (empty-k)) '(1 2 3 4 5) (empty-k)))


;15

(define strange
  (lambda (x)
    ((lambda (g) (lambda (x) (g g)))
     (lambda (g) (lambda (x) (g g))))))

(define strange-cps
  (lambda (x k)
    ((lambda (g k^) (k^ (lambda (x k^^) (g g k^^))))
     (lambda (g k^^^) (k^^^ (lambda (x k^^^^) (g g k^^^^)))) k)))

;16

(define use-of-strange
  (let ([strange^ (((strange 5) 6) 7)])
    (((strange^ 8) 9) 10)))


(define use-of-strange-cps
  ((strange-cps 5 (empty-k)) 10 (empty-k))) ;Both returns a procedure. Not sure if I'm missing something or not.



;17

(define why
  (lambda (f)
    ((lambda (g)
       (f (lambda (x) ((g g) x))))
     (lambda (g)
       (f (lambda (x) ((g g) x)))))))

(define almost-length
    (lambda (f)
      (lambda (ls)
        (if (null? ls)
            0
            (add1 (f (cdr ls)))))))

(define almost-length-cps
    (lambda (f k)
      (k (lambda (ls k^)
        (if (null? ls)
            (k^ 0)
            (f (cdr ls) (lambda(x^) (k^ (add1 x^)))))))))


(define why-cps
  (lambda (f k)
    ((lambda (g k^)
       (f (lambda (x k^^) (g g (lambda(x^) (x^ x  k^^)))) k^))
     (lambda (g k^^^)
       (f (lambda (x k^^^^) (g g (lambda(x^^) (x^^ x k^^^^)))) k^^^)) k)))

#|
((why-cps almost-length-cps (empty-k)) '(a b c d e) (empty-k))
|#
