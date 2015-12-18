#lang racket
(require "parenthec.rkt")


(define-union expr
  (const cexp)
  (var n)
  (if test conseq alt)
  (mult nexp1 nexp2)
  (sub1 nexp)
  (zero nexp)
  (capture body)
  (return kexp vexp)
  (let exp body)              
  (lambda body)
  (app rator rand))

(define-union clos
  (closure body env))

(define-union envr
  (extend-env env^ a^)
  (empty-env))

(define value-of-cps
  (lambda (exp env k)
    (union-case exp expr
      [(const expr) (apply-k k expr)]
      [(mult x1 x2) (value-of-cps x1 env (mult-outer-k x2 env k))]
      [(sub1 x) (value-of-cps x env (sub1-k k))]
      [(zero x) (value-of-cps x env (zero-k k))]
      [(if test conseq alt) (value-of-cps test env (if-k conseq alt env k))]
      [(capture body)  (value-of-cps body (envr_extend-env env k) k)]
      [(return k-exp v-exp) (value-of-cps k-exp env (return-k v-exp env))]
      [(let e body) (value-of-cps e env (let-k body env k))]
      [(var expr) (apply-env env expr k)]
      [(lambda body) (apply-k k (clos_closure body env))] 
      [(app rator rand) (value-of-cps rator env (app-outer-k rand env k))])))


(define return-k
  (lambda(v-exp env)
    `(return ,v-exp ,env)))

(define sub1-k
  (lambda(k^)
    `(sub1-k ,k^)))

(define zero-k
  (lambda(k^)
    `(zero-k ,k^)))



(define if-k
  (lambda(conseq^ alt^ env^ k^)
    `(if-k ,conseq^ ,alt^ ,env^ ,k^)))

(define let-k
  (lambda(body^ env^ k^)
    `(let-k ,body^ ,env^ ,k^)))


(define app-inner-k
  (lambda(v^ k^)
    `(app-inner-k ,v^ ,k^)))


(define app-outer-k
  (lambda(rand^ env^ k^)
    `(app-outer-k ,rand^ ,env^ ,k^)))




(define mult-inner-k
  (lambda (v^ k^)
    `(mult-inner-k ,v^ ,k^)))

(define mult-outer-k
  (lambda(x2^ env^ k^)
    `(mult-outer-k ,x2^ ,env^ ,k^)))



(define apply-k
  (lambda(k v)
    (match k
      [`(empty-k) v]
      [`(sub1-k ,k^) (apply-k k^ (sub1 v))]
      [`(zero-k ,k^) (apply-k k^ (zero? v))]
      [`(if-k ,conseq^ ,alt^ ,env^ ,k^) (if v (value-of-cps conseq^ env^ k^) (value-of-cps alt^ env^ k^))]
      [`(let-k ,body^ ,env^ ,k^) (value-of-cps body^ (envr_extend-env env^ v) k^)]
      [`(app-inner-k ,v^ ,k^) (apply-closure v^ v k^)]
      [`(app-outer-k ,rand^ ,env^ ,k^) (value-of-cps rand^ env^ (app-inner-k v k^))]
      [`(mult-inner-k ,v^ ,k^) (apply-k k^ (* v^ v))]
      [`(mult-outer-k ,x2^ ,env^ ,k^) (value-of-cps x2^ env^ (mult-inner-k v k^))]
      [`(return ,v-exp ,env) (value-of-cps v-exp env v)])))

(define extend-env
  (lambda(env^ a^)
    `(extend-env ,env^ ,a^)))
    

(define apply-env
  (lambda(env y k)
    (union-case env envr
     [(extend-env env^ a^) (if (zero? y) (apply-k k a^) (apply-env env^ (sub1 y) k))]
      [(empty-env) (error 'value-of "unbound identifier")])))

(define closure
  (lambda(body env)
    `(closure ,body ,env)))
    
(define apply-closure
  (lambda(x a k)
    (union-case x clos
    [(closure body env) (value-of-cps body (envr_extend-env env a) k)])))

(define empty-env
  (lambda()
    `(empty-env)))

 
(define empty-k
  (lambda()
  `(empty-k)))


(define main 
  (lambda ()
    (value-of-cps 
     (expr_let 
      (expr_lambda
       (expr_lambda 
        (expr_if
         (expr_zero (expr_var 0))
         (expr_const 1)
         (expr_mult (expr_var 0) (expr_app (expr_app (expr_var 1) (expr_var 1)) (expr_sub1 (expr_var 0)))))))
      (expr_mult
       (expr_capture
        (expr_app
         (expr_app (expr_var 1) (expr_var 1))
         (expr_return (expr_var 0) (expr_app (expr_app (expr_var 1) (expr_var 1)) (expr_const 4)))))
       (expr_const 5)))
     (envr_empty-env)
     (empty-k))))
