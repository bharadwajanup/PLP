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


(define-union kt
  (empty-k)
  (sub1-k k^)
  (zero-k k^)
  (if-k conseq^ alt^ env^ k^)
  (let-k body^ env^ k^)
  (app-inner-k v^ k^)
  (app-outer-k rand^ env^ k^)
  (mult-inner-k v^ k^)
  (mult-outer-k x2^ env^ k^)
  (return-k v-exp env)
  )


(define value-of-cps
  (lambda (exp env k)
    (union-case exp expr
      [(const expr) (let* ((v expr))
                      (apply-k k v))]
      [(mult x1 x2) (let* ((k (kt_mult-outer-k x2 env k))
                           (exp x1))
                      (value-of-cps exp env k))]
      [(sub1 x) (let* ((k (kt_sub1-k k))
                      (exp x))
                (value-of-cps exp env k))]
      [(zero x) (let* ((k (kt_zero-k k))
                      (exp x))
                (value-of-cps exp env k))]
      [(if test conseq alt) (let* ((k (kt_if-k conseq alt env k) )
                                   (exp test))
                            (value-of-cps exp env k))]
      [(capture body)  (let* ((env (envr_extend-env env k))
                              (exp body))
                         (value-of-cps exp env k))]
      [(return k-exp v-exp) (let* ((k (kt_return-k v-exp env))
                                   (exp k-exp))
                              (value-of-cps exp env k))]
      [(let e body) (let* ((k (kt_let-k body env k))(exp e))
                      (value-of-cps exp env k))]
      [(var expr) (let* ((y expr))(apply-env env y k))]
      [(lambda body) (let* ((v (clos_closure body env)))(apply-k k v))] 
      [(app rator rand) (let* ((k (kt_app-outer-k rand env k))(exp rator))(value-of-cps exp env k))])))
;(value-of-cps exp env k))

#|(define return-k
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
    `(mult-outer-k ,x2^ ,env^ ,k^)))|#


;;(let* (()())
(define apply-k
  (lambda(k v)
    (union-case k kt
      [(empty-k) v]
      [(sub1-k k^) (let* ((v (sub1 v))(k k^))(apply-k k v))]
      [(zero-k k^) (let* ((v (zero? v))(k k^))(apply-k k v))]
      [(if-k conseq^ alt^ env^ k^) (if v (let* ((k k^)(env env^)(exp conseq^))
                                           (value-of-cps exp env k)) (let* ((k k^)(env env^)(exp alt^))(value-of-cps exp env k)))]
      [(let-k body^ env^ k^) (let* ((k k^)(env (envr_extend-env env^ v))(exp body^))(value-of-cps exp env k))]
      [(app-inner-k v^ k^) (let* ((k k^)(a v)(x v^))(apply-closure x a k))]
      [(app-outer-k rand^ env^ k^) (let* ((k (kt_app-inner-k v k^))(env env^)(exp rand^))(value-of-cps exp env k))]
      [(mult-inner-k v^ k^) (let* ((k k^)(v (* v^ v)))(apply-k k v))]
      [(mult-outer-k x2^ env^ k^) (let* ((k (kt_mult-inner-k v k^))(env env^)(exp x2^))(value-of-cps exp env k))]
      [(return-k v-exp env) (let* ((k v)(exp v-exp))(value-of-cps exp env k))])))

#|(define extend-env
  (lambda(env^ a^)
    `(extend-env ,env^ ,a^)))|#
    

(define apply-env
  (lambda(env y k)
    (union-case env envr
     [(extend-env env^ a^) (if (zero? y) (let* ((v a^))(apply-k k v)) (let* ((y (sub1 y))(env env^))(apply-env env y k)))]
      [(empty-env) (error 'value-of "unbound identifier")])))

#|(define closure
  (lambda(body env)
    `(closure ,body ,env)))|#
    
(define apply-closure
  (lambda(x a k)
    (union-case x clos
    [(closure body env) (let* ((env (envr_extend-env env a))(exp body))(value-of-cps exp env k))])))

#|(define empty-env
  (lambda()
    `(empty-env)))

 
(define empty-k
  (lambda()
  `(empty-k)))|#


(define main 
  (lambda ()
    (let* ((k (kt_empty-k))(env (envr_empty-env))(exp (expr_let 
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
       (expr_const 5)))))
    (value-of-cps exp env k))))
