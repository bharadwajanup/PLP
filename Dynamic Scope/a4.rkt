#lang racket

;;Part 1

(define acc-gen
  (lambda (y lst)
    (cond
      [(null? lst) (list (cons y '(0)))]
      [(eq? (caar lst) y)  (acc-gen y (cdr lst))]
      [else (cons (cons (caar lst) (list (add1 (car (cdr (car lst)))))) (acc-gen y (cdr lst)))])))

(define lex
  (lambda (exp acc)
    (match exp
      ;[`,x #:when (number? x) (cons 'const (list x))] ;Way easier to implement it without cons. Realised it in this assignment.
      [`,x #:when (number? x)  `(const ,x)]
      ;[`,x #:when (symbol? x) (cons 'var (list (car (cdr (assoc x acc)))))]
      [`,x #:when (symbol? x) `(var ,(car (cdr (assoc x acc))))]
      ;[`(zero? ,n) (cons 'zero? (list (lex n acc)))]
      [`(zero? ,n) `(zero? ,(lex n acc))]
      ;[`(if ,c ,a ,b) (cons 'if (cons (lex c acc) (cons (lex a acc) (lex b acc))))]
      [`(if ,c ,a ,b) `(if ,(lex c acc) ,(lex a acc) ,(lex b acc))]
      [`(* ,a ,b) `(* ,(lex a acc) ,(lex b acc))]
      ;[`(sub1 ,n) (cons 'sub1 (list (lex n acc)))]
      [`(sub1 ,n) `(sub1 ,(lex n acc))]
      ;;[`(let ((,var ,val)) ,body) (cons 'let (list (lex val (acc-gen val acc))))]
      [`(let ((,var ,val)) ,body) `(let ,(lex val (acc-gen var acc)) ,(lex body (acc-gen var acc)))]
      ;[`(lambda (,x) ,body) (cons 'lambda (list (lex body (acc-gen x acc))))]
      [`(lambda (,x) ,body) `(lambda ,(lex body (acc-gen x acc)))]
      [`(,rator ,rand) `(,(lex rator acc) ,(lex rand acc))])))


;;Part-2

(define value-of
  (lambda (exp env)
    (match exp
      [`,x #:when (number? x) x]
      [`,y #:when (symbol? y) (apply-env env y)]
      [`,y #:when (boolean? y) y]
      [`(zero? ,n) (zero? (value-of n env))]
      [`(if ,test ,conseq ,then) (if (value-of test env) (value-of conseq env) (value-of then env))]
      [`(sub1 ,n) (sub1 (value-of n env))]
      [`(+ ,nexp1 ,nexp2) (+ (value-of nexp1 env) (value-of nexp2 env))]
      [`(* ,p ,q) (* (value-of p env) (value-of q env))]
      [`(let ([,x ,y]) ,body) (let ((y (value-of y env)))(value-of body (extend-env x y env)))]  ;;do something which replaces the value of x with y
      [`(lambda (,x) ,body) (lambda (a) (value-of body (extend-env x a env)))]
      [`(,rator ,rand) ((value-of rator env) (value-of rand env))])))

(define empty-env
  (lambda()
    (lambda (y) (error 'value-of "unbound variable ~s" y))))

(define apply-env
  (lambda (env y)
    (env y)))
(define extend-env
  (lambda (x a env)
    (lambda(y) (if (eqv? x y) a (apply-env env y)))))



;;Part 2 The second interpreter
(define value-of-fn
  (lambda (exp env)
    (match exp
      [`,x #:when (number? x) x]
      [`,y #:when (symbol? y) (apply-env-fn env y)]
      [`,y #:when (boolean? y) y]
      [`(zero? ,n) (zero? (value-of-fn n env))]
      [`(if ,test ,conseq ,then) (if (value-of-fn test env) (value-of-fn conseq env) (value-of-fn then env))]
      [`(sub1 ,n) (sub1 (value-of-fn n env))]
      [`(+ ,nexp1 ,nexp2) (+ (value-of-fn nexp1 env) (value-of-fn nexp2 env))]
      [`(* ,p ,q) (* (value-of-fn p env) (value-of-fn q env))]
      ;[`(let ([,x ,y]) ,body) (let ((y (value-of-fn y env)))(value-of-fn body (extend-env-fn x y env)))]
      ;[`(let ([,x ,y]) ,body) (let ((y (value-of-fn y env)))((closure-fn x body env)y))]
      [`(let ([,x ,y]) ,body) (apply-closure-fn (closure-fn x body env) (value-of-fn y env))]
      [`(lambda (,x) ,body) (closure-fn x body env)]
      [`(,rator ,rand) (apply-closure-fn (value-of-fn rator env) (value-of-fn rand env))])))

(define empty-env-fn
  (lambda() 
    (lambda (y) (error 'value-of "unbound variable ~s" y))))

(define apply-env-fn
  (lambda (env y)
    (env y)))
(define extend-env-fn
  (lambda (x a env)
    (lambda(y) (if (eqv? x y) a (apply-env-fn env y)))))


(define apply-closure-fn
  (lambda(rator rand)
    (rator rand)))

(define closure-fn
  (lambda (x body env)
    (lambda (a) (value-of-fn body (extend-env-fn x a env)))))
  

;Part 2 The third interpreter
(define value-of-ds
  (lambda (exp env)
    (match exp
      [`,x #:when (number? x) x]
      [`,y #:when (symbol? y) (apply-env-ds env y)]
      [`,y #:when (boolean? y) y]
      [`(zero? ,n) (zero? (value-of-ds n env))]
      [`(if ,test ,conseq ,then) (if (value-of-ds test env) (value-of-ds conseq env) (value-of-ds then env))]
      [`(sub1 ,n) (sub1 (value-of-ds n env))]
      [`(+ ,nexp1 ,nexp2) (+ (value-of-ds nexp1 env) (value-of-ds nexp2 env))]
      [`(* ,p ,q) (* (value-of-ds p env) (value-of-ds q env))]
      ;[`(let ([,x ,y]) ,body) (let ((y (value-of-ds y env)))(value-of-ds body (extend-env-ds x y env)))]
      ;[`(let ([,x ,y]) ,body) (let ((y (value-of-ds y env)))((closure-ds x body env)y))]
      [`(let ([,x ,y]) ,body) (apply-closure-ds (closure-ds x body env) (value-of-fn y env))]
      [`(lambda (,x) ,body) (closure-ds x body env)]
      [`(,rator ,rand) (apply-closure-ds (value-of-ds rator env) (value-of-ds rand env))])))

(define empty-env-ds
  (lambda()
    (lambda (y) (error 'value-of "unbound variable ~s" y))))

(define apply-env-ds
  (lambda (env y)
    (env y)))
(define extend-env-ds
  (lambda (x a env)
    (lambda(y) (if (eqv? x y) a (apply-env-ds env y)))))

(define closure-ds
  (lambda (x body env)
    `(closure ,x ,body ,env)))

(define apply-closure-ds
  (lambda (cl a)
    (match cl
    [`(closure ,x ,body ,env) (value-of-ds body (extend-env-ds x a env))])))


;;Part 3

(define value-of-dynamic
  (lambda (exp env)
    (match exp
      [`,x #:when (number? x) x]
      [`,y #:when (symbol? y) (apply-env env y)]
      [`,y #:when (boolean? y) y]
      [`(quote ,v) v]
      [`(cons ,a ,b) (cons (value-of-dynamic a env) (value-of-dynamic b env))]
      [`(null? ,a) (null? (value-of-dynamic a env))]
      [`(car ,a) (car (value-of-dynamic a env))]
      [`(cdr ,a) (cdr (value-of-dynamic a env))]
      [`(zero? ,n) (zero? (value-of-dynamic n env))]
      [`(if ,test ,conseq ,then) (if (value-of-dynamic test env) (value-of-dynamic conseq env) (value-of-dynamic then env))]
      [`(sub1 ,n) (sub1 (value-of-dynamic n env))]
      [`(+ ,nexp1 ,nexp2) (+ (value-of-dynamic nexp1 env) (value-of-dynamic nexp2 env))]
      [`(* ,p ,q) (* (value-of-dynamic p env) (value-of-dynamic q env))]
      [`(let ([,x ,y]) ,body) (let ((y (value-of-dynamic y env)))(value-of-dynamic body (extend-env x y env)))]
      [`(lambda (,x) ,body) `(lambda (,x) ,body)]
      [`(,rator ,rand) (match-let ((`(lambda (,x) ,body) (value-of-dynamic rator env))
                                   (a (value-of-dynamic rand env)))
                        (value-of-dynamic body (extend-env x a env)))])))


;;Brainteaser - 4

(define value-of-ri
  (lambda (empty-env extend-env apply-env closure apply-closure)
    (lambda (exp)
      (letrec ((value-of-letrec (lambda (expr env)
                          (match expr
      [`,x #:when (number? x) x]
      [`,y #:when (symbol? y) (apply-env env y)]
      [`,y #:when (boolean? y) y]
      [`(zero? ,n) (zero? (value-of-letrec n env))]
      [`(if ,test ,conseq ,then) (if (value-of-letrec test env) (value-of-letrec conseq env) (value-of-letrec then env))]
      [`(sub1 ,n) (sub1 (value-of-letrec n env))]
      [`(+ ,nexp1 ,nexp2) (+ (value-of-letrec nexp1 env) (value-of-letrec nexp2 env))]
      [`(* ,p ,q) (* (value-of-letrec p env) (value-of-letrec q env))]
      ;[`(let ([,x ,y]) ,body) (let ((y (value-of-letrec y env)))(value-of-letrec body (extend-env x y env)))]
      [`(let ([,x ,y]) ,body) (apply-closure (closure x body env empty-env extend-env apply-env closure apply-closure) (value-of-fn y env))]                      
      [`(lambda (,x) ,body) (closure x body env empty-env extend-env apply-env closure apply-closure)]
      [`(,rator ,rand) (apply-closure (value-of-letrec rator env) (value-of-letrec rand env))]))))
                        (value-of-letrec exp empty-env)))))
    


(define apply-closure-fn-ri
  (lambda(rator rand)
    (rator rand)))

(define closure-fn-ri
  (lambda (x body env empty-env extend-env apply-env closure apply-closure)
    (lambda (a) ((value-of-ri (extend-env x a env) extend-env apply-env closure apply-closure) body))))

(define closure-ds-ri
  (lambda (x body env empty-env extend-env apply-env closure apply-closure)
    `(closure ,x ,body ,env ,empty-env ,extend-env ,apply-env ,closure ,apply-closure)))

(define apply-closure-ds-ri
  (lambda (cl a)
    (match cl
    [`(closure ,x ,body ,env ,empty-env ,extend-env ,apply-env ,closure ,apply-closure) ((value-of-ri (extend-env-ds x a env) extend-env apply-env closure apply-closure) body)])))




