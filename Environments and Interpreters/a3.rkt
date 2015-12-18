#lang racket

;Part 1 - The first Interpreter
(define value-of
  (lambda (exp env)
    (match exp
      [`,x #:when (number? x) x]
      [`,y #:when (symbol? y)  (unbox (env y))]
      [`,y #:when (boolean? y) y]
      [`(zero? ,n) (zero? (value-of n env))]
      [`(+ ,nexp1 ,nexp2) (+ (value-of nexp1 env) (value-of nexp2 env))]
      [`(if ,test ,conseq ,then) (if (value-of test env) (value-of conseq env) (value-of then env))]
      [`(sub1 ,n) (sub1 (value-of n env))]
      [`(* ,p ,q) (* (value-of p env) (value-of q env))]
      [`(let ([,x ,y]) ,body) (let ([y (box (value-of y env))])(value-of body (lambda(a) (if (eqv? x a)  y (env a)))))]
      [`(lambda (,x) ,body) (lambda (a) (let ([a (box a)])(value-of body (lambda(y) (if (eqv? x y) a  (env y))))))]
      [`(,rator ,rand) ((value-of rator env)  (value-of rand env))]
      [`(begin2 ,exp1 ,exp2) (begin (value-of exp1 env) (value-of exp2 env))]
      [`(set! ,x ,val2)  (set-box! (env x) (value-of val2 env))])))

;;The solution for set! was collaborated. I was trying to do a (box a) in my "lambda (x) body" case and it wasn't working.
;;The hint to box it before was given by Audit Shah.

;Part 1 - The second Interpreter

(define value-of-fn
  (lambda (exp env)
    (match exp
      [`,x #:when (number? x) x]
      [`,y #:when (symbol? y) (apply-env-fn env y)]
      [`,y #:when (boolean? y) y]
      [`(zero? ,n) (zero? (value-of-fn n env))]
      [`(if ,test ,conseq ,then) (if (value-of-fn test env) (value-of-fn conseq env) (value-of-fn then env))]
      [`(sub1 ,n) (sub1 (value-of-fn n env))]
      [`(+ ,nexp1 ,nexp2) (+ (value-of nexp1 env) (value-of nexp2 env))]
      [`(* ,p ,q) (* (value-of-fn p env) (value-of-fn q env))]
      [`(let ([,x ,y]) ,body) (value-of-fn body (extend-env-fn x (value-of-fn y env) env))]  ;;do something which replaces the value of x with y
      [`(lambda (,x) ,body) (lambda (a) (value-of-fn body (extend-env-fn x a env)))]
      [`(,rator ,rand) ((value-of-fn rator env) (value-of-fn rand env))])))

(define empty-env-fn
  (lambda()
    (lambda (y) (error 'value-of "unbound variable ~s" y))))

(define apply-env-fn
  (lambda (env y)
    (env y)))
(define extend-env-fn
  (lambda (x a env)
    (lambda(y) (if (eqv? x y) a (apply-env-fn env y)))))


;Part-1 - The third Interpreter
(define value-of-ds
  (lambda (exp env)
    (match exp
      [`,x #:when (number? x) x]
      [`,y #:when (symbol? y) (apply-env-ds env y)]
      [`,y #:when (boolean? y) y]
      [`(zero? ,n) (zero? (value-of-ds n env))]
      [`(if ,test ,conseq ,then) (if (value-of-ds test env) (value-of-ds conseq env) (value-of-ds then env))]
      [`(sub1 ,n) (sub1 (value-of-ds n env))]
      [`(+ ,nexp1 ,nexp2) (+ (value-of nexp1 env) (value-of nexp2 env))]
      [`(* ,p ,q) (* (value-of-ds p env) (value-of-ds q env))]
      [`(let ([,x ,y]) ,body) (value-of-ds body (extend-env-ds x (value-of-ds y env) env))]  ;;do something which replaces the value of x with y
      [`(lambda (,x) ,body) (lambda (a) (value-of-ds body (extend-env-ds x a env)))]
      [`(,rator ,rand) ((value-of-ds rator env) (value-of-ds rand env))])))

(define empty-env-ds
  (lambda()
    '(empty-env-ds)))

(define extend-env-ds
  (lambda (id arg env)
  `(extend-env-ds ,id ,arg ,env)))

(define apply-env-ds
  (lambda (env y)
    (match env
     ; ['() (empty-env-ds)]
      [`(extend-env-ds ,id ,arg ,env) (if (eq? id y) arg (apply-env-ds env y))]
      [`(empty-env-ds)  (error 'value-of "unbound variable ~s" y)] 
      [else (else y)]
      )))

;Part-2 "fo-eulav"

(define fo-eulav
  (lambda (exp env)
    (match exp
      [`,x #:when (number? x) x]
      [`,y #:when (symbol? y) (env y)]
      [`,y #:when (boolean? y) y]
      [`(,n ?orez) (zero? (fo-eulav n env))]
      [`(,then ,conseq ,test fi) (if (fo-eulav test env) (fo-eulav conseq env) (fo-eulav then env))]
      [`(,n 1bus) (sub1 (fo-eulav n env))]
      [`(,q ,p *) (* (fo-eulav p env) (fo-eulav q env))]
      [`(,body (,x) adbmal) (lambda (a) (fo-eulav body (lambda(y) (if (eqv? x y) a (env y)))))]
      [`(,rator ,rand) ((fo-eulav rand env) (fo-eulav rator env))])))


;6
(define value-of-lex
  (lambda (exp env)
    (match exp
      ((? boolean?) exp)
      ((? number?) exp)
      (`(sub1 ,body) (sub1 (value-of-lex body env)))
      (`(zero? ,body) (zero? (value-of-lex body env)))
      (`(* ,n1 ,n2) (* (value-of-lex n1 env) (value-of-lex n2 env)))
      (`(if ,t ,c ,a) (if (value-of-lex t env) (value-of-lex c env) (value-of-lex a env)))
      (`(var ,num) (apply-env-lex env num))
      (`(lambda ,body) (lambda (a) (value-of-lex body (extend-env-lex a env))))
      (`(,rator ,rand) ((value-of-lex rator env) (value-of-lex rand env))))))

(define empty-env-lex 
  (lambda () '()))
(define apply-env-lex value-of-lex) 
(define extend-env-lex value-of)
