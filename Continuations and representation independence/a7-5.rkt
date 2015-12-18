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
      [`(mult ,x1 ,x2) (value-of-cps x1 env (lambda(x^) (apply-k k (value-of-cps x2 env (lambda(x^^) (* x^ x^^))))))]
      [`(sub1 ,x) (value-of-cps x env (lambda(x^) (apply-k k (sub1 x^))))]
      [`(zero ,x) (value-of-cps x env (lambda(x^) (apply-k k (zero? x^))))]
      [`(if ,test ,conseq ,alt) (value-of-cps test env (lambda(x^) (if x^ (value-of-cps conseq env k) (value-of-cps alt env k))))]
      [`(capture ,body)  (value-of-cps body (extend-env env k) k)]
      [`(return ,k-exp ,v-exp) (value-of-cps k-exp env (lambda(x^)  (value-of-cps v-exp env (lambda(x^^)  (x^ x^^)))))]
      [`(let ,e ,body) (value-of-cps e env (lambda(x^)
                         (value-of-cps body (extend-env env x^) k)))]
      [`(var ,expr) (apply-env env expr k)]
      [`(lambda ,body) (apply-k k (lambda (a k^) (value-of-cps body (extend-env env a) k^)))]
      [`(app ,rator ,rand) (value-of-cps rator env (lambda(x^) (value-of-cps rand env (lambda(x^^)  (apply-closure x^ x^^ k)))))])))




(define apply-closure
  (lambda(rator rand k)
    (rator rand k)))

(define apply-k
  (lambda(k expr)
    (k expr)))

(define extend-env
  (lambda(env^ a^)
    `(extend-env ,env^ ,a^)))
    ;(lambda (y k) (if (zero? y) (apply-k k a^) (apply-env env^ (sub1 y) k)))))

(define apply-env
  (lambda(env y k)
    (match env
     [`(extend-env ,env^ ,a^) (if (zero? y) (apply-k k a^) (apply-env env^ (sub1 y) k))]
      [`(empty-env) (error 'value-of "unbound identifier")]
      [else (env y k)])))


    


 



