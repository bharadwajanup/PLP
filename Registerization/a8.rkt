#lang racket
(define m 'hukarz)
(define n 'hukarz)
(define ls 'hukarz)
(define k 'hukarz)
(define v 'hukarz)

;1 assuming the given functions are cps-ed
(define ack
  (lambda ();;m n k
    (cond
      [(zero? m) (begin (set! v (add1 n))
                   (apply-k))]
      [(zero? n) (begin (set! n 1)
                        (set! m (sub1 m))
                   (ack))]
      [else (begin (set! k (ack-k m k))
                   (set! n (sub1 n))
              (ack))])))

(define ack-k
  (lambda(m^ k^)
    `(ack-k ,m^ ,k^)))



;2
(define depth
  (lambda ();;ls k
    (cond
      [(null? ls) (begin (set! v 1)
                    (apply-k))]
      [(pair? (car ls))
       (begin (set! k (depth-outer-k ls k))
              (set! ls (car ls))
         (depth))]
      [else (begin (set! ls (cdr ls))
              (depth))])))


(define depth-inner-k
  (lambda(l^ k^)
    `(depth-inner-k ,l^ ,k^)))

(define depth-outer-k
  (lambda(ls^ k^)
    `(depth-outer-k ,ls^ ,k^)))



;3 
(define fact
  (lambda ();n k
   ;; ((lambda (fact)
     ;;  (fact fact))
     ;;(lambda (fact)
       (cond
         [(zero? n) (begin (set! v 1)
                      (apply-k))]
         [else (begin (set! k (fact-k n k))
                      (set! n (sub1 n))
                      (fact))]))
     );))

(define fact-k
  (lambda(n^ k^)
   `(fact-k ,n^ ,k^) ))
;(lambda (v) (apply-k k^ (* n^ v))



;4
(define pascal
  (lambda ();n k
             (begin 
               	  (cond
		    [(> m n) (begin (set! v '()) ;;Assigning registers to m and a (I'm using register ls) seems to work. So removed the let bindings and the lambda as I'm setting the values to a register.
                               (apply-k))]
		    [else
                            (begin
                              (set! ls (+ ls m))
                              (set! k (pascal-k ls k))
                              (set! m (add1 m))
			    (pascal))]))))




(define pascal-k
  (lambda(a^ k^)
    `(pascal-k ,a^ ,k^)))
   ; (lambda (v) (apply-k k^ (cons a^ v)))))




(define empty-k
  (lambda()`(empty-k)))#|(lambda ()
    (lambda (v)
      v)))|#

(define apply-k
  (lambda();k v
    (match k
      [`(empty-k) v]
      [`(ack-k ,m^ ,k^) (begin (set! k k^)
                               (set! n v)
                               (set! m (sub1 m^))
                          (ack))]
      [`(depth-inner-k ,l^ ,k^) (let ((l (add1 l^)))
                                  (if (< l v) (begin (set! k k^)
                                                (apply-k))
                                      (begin (set! v l)
                                             (set! k k^)
                                          (apply-k))))]
      [`(depth-outer-k ,ls^ ,k^) (begin (set! k (depth-inner-k v k^))
                                        (set! ls (cdr ls^))
                                   (depth))]
      [`(fact-k ,n^ ,k^) (begin (set! v (* n^ v))
                                (set! k k^)
                           (apply-k))]
      [`(pascal-k ,a^ ,k^) (begin (set! v (cons a^ v))
                                        (set! k k^)
                                        (apply-k))])))
   ; [else (k v)])))


(define ack-reg-driver
  (lambda (m^ n^)
    (begin 
  (set! k (empty-k))
  (set! m m^)
  (set! n n^)
  (ack))))

(define depth-reg-driver
  (lambda(ls^)
    (begin
      (set! k (empty-k))
      (set! ls ls^)
      (depth))))

(define fact-reg-driver
  (lambda(n^)
    (begin
      (set! k (empty-k))
      (set! n n^)
      (fact))))

(define pascal-reg-driver
  (lambda(n^)
    (begin
      (set! k (empty-k))
      (set! n n^)
      (set! m 1) ;;Vales of a and m put into a register.
      (set! ls 0)	
      (pascal))))


;Gives the zero indexed nth element in the list - not needed anymore
#|(define nth-element
  (lambda(n lst)
    (cond
      [(null? lst) '()]
      [(= n 0) (car lst)]
      [else (nth-element (sub1 n) (cdr lst))])))|#

(define empty-k-fib
  (lambda(jumpout)
    (lambda(v) (jumpout v))))

(define fib
  (lambda (n k)
  (cond
    [(= n 0) (lambda() (apply-k-fib k 1))] ;;setting as 0 passes the test cases but in the assignment description, it is given as 1.
    [(= n 1) (lambda() (apply-k-fib k 1))]
    [else (lambda() (fib (sub1 n) (fib-outer-k n k)))])))


(define fib-outer-k
  (lambda(n^ k^)
    (lambda(v) (fib (sub1 (sub1 n^)) (fib-inner-k v k^)))))
(define fib-inner-k
  (lambda(u^ k^)
    (lambda(v) (k^ (+ u^ v)))))
(define apply-k-fib
  (lambda(k v)
    (k v)))




;Still ended up in infinite loops
;(define rampoline (lambda (n1 n2 n3) (rampoline ((nth-element (random 3) `(,n1 ,n2 ,n3))) ((nth-element (random 3) `(,n1 ,n2 ,n3))) ((nth-element (random 3) `(,n1 ,n2 ,n3))))))



;This didnt work either..
;(define rampoline (lambda (n1 n2 n3) (rampoline ((nth-element (car (unique-rand-list (random 3) '())) `(,n1 ,n2 ,n3))) ((nth-element (car (cdr (unique-rand-list (random 3) '()))) `(,n1 ,n2 ,n3))) ((nth-element (car (cdr (cdr (unique-rand-list (random 3) '())))) `(,n1 ,n2 ,n3))))))


(define rampoline (lambda (n1 n2 n3) (cond
                                       [(= (random 3) 0) (rampoline (n1) n2 n3)]
                                       [(= (random 3) 1) (rampoline n1 (n2) n3)]
                                       [(= (random 3) 2) (rampoline n1 n2 (n3))]
                                       [else (rampoline n1 n2 n3)])))




#| Not needed anymore
(define len
  (lambda(ls)
    (cond
      [(null? ls) 0]
      [else (add1 (len (cdr ls)))])))

(define unique-rand-list
  (lambda(n ls)
    (cond
      [(= (len ls) 3) ls]
      [(not (memv n ls)) (unique-rand-list (random 3) (cons n ls))]
      [else (unique-rand-list (random 3) ls)])))|#

(define fib-ramp-driver
  (lambda (n1 n2 n3)
    (call/cc
      (lambda (jumpout)
	(rampoline
	    (lambda ()
	      (fib n1 (empty-k-fib jumpout)))
	    (lambda ()
	      (fib n2 (empty-k-fib jumpout)))
            (lambda ()
	      (fib n3 (empty-k-fib jumpout))))))))


(define trampoline
  (lambda(th)
    (trampoline (th)))) 

