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
  #|(lambda ()
    (lambda (y)
      (error 'value-of "unbound identifier"))))|#
 
(define empty-k
  (lambda ()
    (lambda (v)
      v)))



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
    (lambda(v)  (value-of-cps v-exp env v))))

(define sub1-k
  (lambda(k^)
    (lambda(v) (apply-k k^ (sub1 v)))))

(define zero-k
  (lambda(k^)
    (lambda(v) (apply-k k^ (zero? v)))))

(define if-k
  (lambda(conseq^ alt^ env^ k^)
    (lambda(v) (if v (value-of-cps conseq^ env^ k^) (value-of-cps alt^ env^ k^)))))

(define let-k
  (lambda(body^ env^ k^)
    (lambda(v) (value-of-cps body^ (extend-env env^ v) k^))))

(define app-inner-k
  (lambda(v^ k^)
    (lambda(v)  (apply-closure v^ v k^))))

(define app-outer-k
  (lambda(rand^ env^ k^)
    (lambda(v) (value-of-cps rand^ env^ (app-inner-k v k^)))))




(define mult-inner-k
  (lambda (v^ k^)
    (lambda(v)
      (apply-k k^ (* v^ v)))))

(define mult-outer-k
  (lambda(x2^ env^ k^)
    (lambda(v)  (value-of-cps x2^ env^ (mult-inner-k v k^)))))



(define apply-k
  (lambda(k v)
    (k v)))

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

    


 



