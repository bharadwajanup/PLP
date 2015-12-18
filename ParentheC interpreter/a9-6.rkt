#lang racket
(require "parenthec.rkt")

(define-registers k v n exp env y x a)

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

;set! 
(define value-of-cps
  (lambda () ;exp env k
    (union-case exp expr
      [(const expr) (begin
                      (set! v expr)
                      (apply-k))]
      [(mult x1 x2) (begin
                      (set! k (kt_mult-outer-k x2 env k))
                      (set! exp x1)
                      (value-of-cps))]
      [(sub1 x^) (begin
                  (set! k (kt_sub1-k k))
                  (set! exp x^)
                  (value-of-cps))]
      [(zero x^) (begin
                  (set! k (kt_zero-k k))
                  (set! exp x^)
                  (value-of-cps))]
      [(if test conseq alt) (begin
                              (set! k (kt_if-k conseq alt env k) )
                              (set! exp test)
                              (value-of-cps))]
      [(capture body)  (begin (set! env (envr_extend-env env k))
                              (set! exp body)
                              (value-of-cps))]
      [(return k-exp v-exp) (begin (set! k (kt_return-k v-exp env))
                                   (set! exp k-exp)
                                   (value-of-cps))]
      [(let e body) (begin (set! k (kt_let-k body env k))
                           (set! exp e)
                           (value-of-cps))]
      [(var expr) (begin (set! y expr)
                         (apply-env))]
      [(lambda body) (begin (set! v (clos_closure body env))
                            (apply-k))] 
      [(app rator rand) (begin (set! k (kt_app-outer-k rand env k))
                               (set! exp rator)
                               (value-of-cps))])))
;(value-of-cps))

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


;;(begin ()())
(define apply-k
  (lambda() ;k v
    (union-case k kt
      [(empty-k) v]
      [(sub1-k k^) (begin
                     (set! v (sub1 v))
                     (set! k k^)
                     (apply-k))]
      [(zero-k k^) (begin
                     (set! v (zero? v))
                     (set! k k^)
                     (apply-k))]
      [(if-k conseq^ alt^ env^ k^) (if v (begin
                                           (set! k k^)
                                           (set! env env^)
                                           (set! exp conseq^)
                                           (value-of-cps)) (begin
                                                             (set! k k^)
                                                             (set! env env^)
                                                             (set! exp alt^)
                                                             (value-of-cps)))]
      [(let-k body^ env^ k^) (begin
                               (set! k k^)
                               (set! env (envr_extend-env env^ v))
                               (set! exp body^)
                               (value-of-cps))]
      [(app-inner-k v^ k^) (begin
                             (set! k k^)
                             (set! a v)
                             (set! x v^)
                             (apply-closure))]
      [(app-outer-k rand^ env^ k^) (begin
                                     (set! k (kt_app-inner-k v k^))
                                     (set! env env^)
                                     (set! exp rand^)
                                     (value-of-cps))]
      [(mult-inner-k v^ k^) (begin
                              (set! k k^)
                              (set! v (* v^ v))
                              (apply-k))]
      [(mult-outer-k x2^ env^ k^) (begin
                                    (set! k (kt_mult-inner-k v k^))
                                    (set! env env^)
                                    (set! exp x2^)
                                    (value-of-cps))]
      [(return-k v-exp env^) (begin
                              (set! k v)
                              (set! exp v-exp)
                              (value-of-cps))])))

#|(define extend-env
  (lambda(env^ a^)
    `(extend-env ,env^ ,a^)))|#
    

(define apply-env
  (lambda() ;;env y k
    (union-case env envr
     [(extend-env env^ a^) (if (zero? y) (begin
                                           (set! v a^)
                                           (apply-k)) (begin (set! y (sub1 y))
                                                             (set! env env^)
                                                             (apply-env)))]
      [(empty-env) (error 'value-of "unbound identifier")])))

#|(define closure
  (lambda(body env)
    `(closure ,body ,env)))|#
    
(define apply-closure
  (lambda() ;;x a k
    (union-case x clos
    [(closure body env^) (begin (set! env (envr_extend-env env^ a))
                               (set! exp body)
                               (value-of-cps))])))

#|(define empty-env
  (lambda()
    `(empty-env)))

 
(define empty-k
  (lambda()
  `(empty-k)))|#


(define main 
  (lambda ()
    (begin (set! k (kt_empty-k))
           (set! env (envr_empty-env))
           (set! exp (expr_let 
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
       (expr_const 5))))
    (value-of-cps))))
