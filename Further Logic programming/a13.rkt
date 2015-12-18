#lang racket
(require "mk.rkt")
(require "numbers.rkt")



;1

#(define (listo ls)
  (fresh (a d)
         (conde
          [(== '() ls)]
          [(== `(,a . ,d) ls)
           (== d '())]
          [(== `(,a . ,d) ls)
           (=/= d '()) (listo d)])))

(define listo
  (lambda(ls)
    (fresh (a d)
    (conde
     
     [(== ls `(,a . ,d))
      (== d '())]
     [(== ls `(,a . ,d))
     (listo d)]))))


;2

(define (facto n out)
  (conde
   [(== n (build-num 0)) (== out (build-num 1))]
   [(fresh (res prev)
           (minuso n '(1) prev)
           (facto prev res)
           (*o n res out))])) 

;3
(define fibs
    (lambda (n)
      (cond
        ((eqv? n 0) (values 1 1))
        (else
         (let ((n- (- n 1)))
           (let-values (((u v) (fibs n-)))
             (let ((u+v (+ u v)))
               (values v u+v))))))))

(define (fibso n o1 o2)
  (conde
   [(== n (build-num 0)) (== o1 (build-num 1)) (== o2 (build-num 1))]
   [(fresh (n- u v uplusv)
           (minuso n '(1) n-)
           (fibso n- u v)
           (pluso u v uplusv)
           (== o1 v)
           (== o2 uplusv))]))

;4

(define (reverseo ls out)
  (conde
   [(== '() ls) (== '() out)]
   [(fresh (a d res)
    (== `(,a . ,d) ls)
     (reverseo d res)
    (appendo res `(,a) out))]))

(define palo
  (lambda(ls out)
    (fresh (rev)
           
    (== ls rev)
    (reverseo ls rev))))


(define (lookup x vars vals o)
  (fresh (y vars^ a vals^)
    (== `(,y . ,vars^) vars)
    (== `(,a . ,vals^) vals)
    (conde
      ((== x y) (== o a))
      ((=/= x y) (lookup x vars^ vals^ o)))))

(define (valof* exps vars vals o)
  (conde
    ((== `() exps) (== o `()))
    ((fresh (exp exps^)
       (== exps `(,exp . ,exps^))
       (fresh (v v^)
         (== o `(,v . ,v^))
         (val-ofo exp vars vals v)
         (valof* exps^ vars vals v^))))))

(define (fo-lavo* exps vars vals o)
  (conde
    ((== `() exps) (== o `()))
    ((fresh (exp exps^)
       (== exps `(,exp . ,exps^))
       (fresh (v v^)
         (== o `(,v . ,v^))
         (fo-lavo exp vars vals v)
         (fo-lavo* exps^ vars vals v^))))))



;fo-lavo
(define (fo-lavo exp vars vals o)
  (conde
;;  ((numbero exp) (== o exp))
    ((symbolo exp) (lookup exp vars vals o))
    ((== exp `(,o etouq))
     (absento 'closure o)
     (absento 'etouq vars))
    ((fresh (rev_exp exps out)
            (reverseo exp rev_exp)
       (== `(tsil . ,exps) rev_exp)
      (absento 'tsil vars)
       (fo-lavo* exps vars vals out)
       (reverseo out o)))
    ((fresh (x b)
       (== exp `(,b (,x) adbmal))
       
       (symbolo x)
       (absento 'adbmal vars)
       (== o `(closure ,x ,b ,vars ,vals))))
    ((fresh (rator rand)
       (== exp `(,rand ,rator))
      (fresh (x b vars^ vals^ a)
        (fo-lavo rator vars vals `(closure ,x ,b ,vars^ ,vals^))
        (fo-lavo rand vars vals a)
        (fo-lavo b `(,x . ,vars^) `(,a . ,vals^) o))))))





;val-ofo
(define (val-ofo exp vars vals o)
  (conde
;;  ((numbero exp) (== o exp))
    ((symbolo exp) (lookup exp vars vals o))
    ((== exp `(quote ,o))
     (absento 'closure o)
     (absento 'quote vars))
    ((fresh (exps)
       (== exp `(list . ,exps))
       (absento 'list vars)
       (valof* exps vars vals o)))
    ((fresh (x b)
       (== exp `(lambda (,x) ,b))
       (absento 'lambda vars)
       (symbolo x)
       (== o `(closure ,x ,b ,vars ,vals))))
    ((fresh (rator rand)
       (== exp `(,rator ,rand))
      (fresh (x b vars^ vals^ a)
        (val-ofo rator vars vals `(closure ,x ,b ,vars^ ,vals^))
        (val-ofo rand vars vals a)
        (val-ofo b `(,x . ,vars^) `(,a . ,vals^) o))))))




;5

(define (membero a ls)
  (fresh (m n)
  (conde
          [(== `(,m . ,n) ls)
          (== a m)]
          [(== `(,m . ,n) ls)
           (membero a n)])))



(define color-middle-earth
  (lambda (colors)
    (run 1 (q) (four-color colors q))))

(define four-color
  (lambda (ls o)
    (fresh (c0 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10)
           (== `((lindon . ,c0) (forodwaith . ,c1) (eriador . ,c2)
                 (rhovanion . ,c3) (enedwaith . ,c4) (rohan . ,c5)
                 (gondor . ,c6) (rhun . ,c7) (mordor . ,c8)
                 (khand . ,c9) (harad . ,c10)) o)
           (absento c0 c2) (absento c0 c1) (absento c1 c3) (absento c1 c2) (absento c2 c3)
           (absento c2 c4) (absento c3 c4) (absento c3 c5) (absento c3 c7) (absento c4 c5)
           (absento c4 c6) (absento c5 c7) (absento c5 c6) (absento c5 c8) (absento c6 c8)
           (absento c7 c9) (absento c7 c8) (absento c8 c9) (absento c8 c10) (absento c9 c10)
           (membero c0 ls) (membero c1 ls) (membero c2 ls) (membero c3 ls)
           (membero c4 ls) (membero c5 ls) (membero c6 ls) (membero c7 ls)
           (membero c8 ls) (membero c9 ls) (membero c10 ls))))


(define (reverzo ls out)
  (conde
   [(== ls '()) (== ls out)]
   [(fresh (a d res)
    (== `(,a . ,d) ls)
    (reverzo d res)
    (appendo res `(,a) out))]))
    
  




   