#lang racket

(define last-non-zero
  (lambda (ls)
    (call/cc
      (lambda (k)
        (letrec
          ((last-non-zero
            (lambda (ls)
              (cond
                [(null? ls) '()]
                [(equal? 0 (car ls))  (k (last-non-zero (cdr ls)))]
                [(not (equal? (car ls) 0)) (cons (car ls) (last-non-zero (cdr ls)))]
                [else (last-non-zero (cdr ls))])
	     )))
          (last-non-zero ls))))))

(define acc-gen
  (lambda (y lst)
    (cond
      [(null? lst) (list (cons y '(0)))]
      [(eq? (caar lst) y)  (acc-gen y (cdr lst))]
      [else (cons (cons (caar lst) (list (add1 (car (cdr (car lst)))))) (acc-gen y (cdr lst)))])))

(define lex
  (lambda (exp acc)
    (match exp
      
      [`,x #:when (number? x)  `(const ,x)]
      [`,x #:when (symbol? x) `(var ,(car (cdr (assoc x acc))))]
      [`(zero? ,nexp) `(zero ,(lex nexp acc))]
      [`(if ,c ,a ,b) `(if ,(lex c acc) ,(lex a acc) ,(lex b acc))]
      [`(* ,nexp1 ,nexp2) `(mult ,(lex nexp1 acc) ,(lex nexp2 acc))]
      [`(sub1 ,n) `(sub1 ,(lex n acc))]
      [`(let ((,var ,val)) ,body) `(let ,(lex val (acc-gen var acc)) ,(lex body (acc-gen var acc)))]
      [`(capture ,x ,body) `(capture ,(lex body (acc-gen x acc)))]
      [`(return ,k-exp ,v-exp) `(return ,(lex k-exp acc) ,(lex v-exp acc))]
      [`(lambda (,x) ,body) `(lambda ,(lex body (acc-gen x acc)))]
      [`(,rator ,rand) `(app ,(lex rator acc) ,(lex rand acc))])))

(define value-of
  (lambda (expr env)
    (match expr
      [`(const ,expr) expr]
      [`(mult ,x1 ,x2) (* (value-of x1 env) (value-of x2 env))]
      [`(sub1 ,x) (sub1 (value-of x env))]
      [`(zero ,x) (zero? (value-of x env))]
      [`(if ,test ,conseq ,alt) (if (value-of test env)
                                    (value-of conseq env)
                                    (value-of alt env))]
      [`(capture ,body) (call/cc (lambda (k)
                                   (value-of body (lambda (y) (if (zero? y) k (env (sub1 y)))))))]
      [`(return ,k-exp ,v-exp) ((value-of k-exp env) (value-of v-exp env))]
      [`(let ,e ,body) (let ((a (value-of e env)))
                         (value-of body (lambda (y) (if (zero? y) a (env (sub1 y))))))]
      [`(var ,expr) (env expr)]
      [`(lambda ,body) (lambda (a) (value-of body (lambda (y) (if (zero? y) a (env (sub1 y))))))]
      [`(app ,rator ,rand) ((value-of rator env) (value-of rand env))])))
 
(define empty-env
  (lambda()
    `(empty-env)))

 
(define empty-k
  (lambda()
  `(empty-k)))




(define value-of-cps
  (lambda (expr env k)
    (match expr
      [`(const ,expr) (apply-k k expr)]
      [`(mult ,x1 ,x2) (value-of-cps x1 env (mult-outer-k x2 env k))]
      [`(sub1 ,x) (value-of-cps x env (sub1-k k))]
      [`(zero ,x) (value-of-cps x env (zero-k k))]
      [`(if ,test ,conseq ,alt) (value-of-cps test env (if-k conseq alt env k))]
      [`(capture ,body)  (value-of-cps body (extend-env env k) k)]
      [`(return ,k-exp ,v-exp) (value-of-cps k-exp env (return-k v-exp env))]
      [`(let ,e ,body) (value-of-cps e env (let-k body env k))]
      [`(var ,expr) (apply-env env expr k)]
      [`(lambda ,body) (apply-k k (closure body env))] 
      [`(app ,rator ,rand) (value-of-cps rator env (app-outer-k rand env k))])))


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
      [`(let-k ,body^ ,env^ ,k^) (value-of-cps body^ (extend-env env^ v) k^)]
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
    (match env
     [`(extend-env ,env^ ,a^) (if (zero? y) (apply-k k a^) (apply-env env^ (sub1 y) k))]
      [`(empty-env) (error 'value-of "unbound identifier")])))

(define closure
  (lambda(body env)
    `(closure ,body ,env)))
    
(define apply-closure
  (lambda(x a k)
    (match x
    [`(closure ,body ,env) (value-of-cps body (extend-env env a) k)])))



(define-syntax cons$
  (syntax-rules ()
    ((cons$ x y) (cons x (delay y)))))
 
(define car$ car)
 
(define cdr$
  (lambda ($) (force (cdr $))))

(define take$
  (lambda (n $)
    (cond
      ((zero? n) '())
      (else (cons (car$ $) 
              (let ((n- (sub1 n)))
                (cond
                  ((zero? n-) '())
                  (else (take$ n- (cdr$ $))))))))))


    (define inf-1s (cons$ 1 inf-1s))





   (define trib$
     (letrec((cal-trib (lambda (a b c)
                        (cons$ a (cal-trib b c (+ (+ a b) c))))))(cal-trib 0 1 1)))



