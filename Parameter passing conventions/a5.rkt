#lang racket

;Taken from the assignment description
(define value-of
  (lambda (exp env)
    (match exp
      [`,b #:when (boolean? b) b]
      [`,n #:when (number? n)  n]
      [`(zero? ,n) (zero? (value-of n env))]
      [`(sub1 ,n) (sub1 (value-of n env))]
      [`(* ,n1 ,n2) (* (value-of n1 env) (value-of n2 env))]
      [`(if ,test ,conseq ,alt) (if (value-of test env)
                                  (value-of conseq env)
                                  (value-of alt env))]
      [`(begin2 ,e1 ,e2) (begin (value-of e1 env) (value-of e2 env))]
      [`(random ,n) (random (value-of n env))]
      [`,y (symbol? y) (apply-env env y)]
      [`(lambda (,x) ,body) (closure x body env)]
      [`(,rator ,rand) (apply-closure (value-of rator env)
                                      (value-of rand env))])))

(define empty-env
  (lambda()
    (lambda (y) (error "unbound variable ~s" y))))

(define apply-env
  (lambda (env y)
    (env y)))

(define extend-env
  (lambda (x a env)
    (lambda(y) (if (eqv? x y) a (apply-env env y)))))

(define apply-closure
  (lambda(rator rand)
    (rator rand)))

(define closure
  (lambda (x body env)
    (lambda (a) (value-of body (extend-env x a env)))))


(define val-of-cbv
  (lambda (exp env)
    (match exp
      [`,b #:when (boolean? b) b]
      [`,n #:when (number? n)  n]
      [`,y #:when (symbol? y) (unbox (apply-env env y))]
     
      [`(zero? ,n) (zero? (val-of-cbv n env))]
      [`(null? ,n) (null? (val-of-cbv n env))]
      [`(sub1 ,n) (sub1 (val-of-cbv n env))]
      [`(add1 ,n) (add1 (val-of-cbv n env))]
      [`(quote ,v) v]
      [`(* ,n1 ,n2) (* (val-of-cbv n1 env) (val-of-cbv n2 env))]
      [`(if ,test ,conseq ,alt) (if (val-of-cbv test env)
                                  (val-of-cbv conseq env)
                                  (val-of-cbv alt env))]
      [`(begin2 ,e1 ,e2) (begin (val-of-cbv e1 env) (val-of-cbv e2 env))]
      [`(random ,n) (random (val-of-cbv n env))]
      [`(cons ,a ,b) (cons (val-of-cbv a env) (val-of-cbv b env))]
      [`(car ,a) (car (val-of-cbv a env))]
      [`(cdr ,a) (cdr (val-of-cbv a env))]
      [`(cons^ ,a ,b) (cons (box (lambda() (val-of-cbv a env))) (box (lambda()(val-of-cbv  b env))))]
      [`(car^ ,a) ((unbox (car (val-of-cbv a env))))] 
      [`(cdr^ ,a) ((unbox (cdr (val-of-cbv a env))))] 
      [`(let ([,x ,y]) ,body) (let ((y (box (val-of-cbv y env))))(val-of-cbv body (extend-env x y env)))]
      [`(lambda (,x) ,body) (closure-cbv x body env)]
      [`(,rator ,y) #:when(symbol? y) (apply-closure (val-of-cbv rator env) (box (unbox (apply-env env y))))]
      [`(set! ,y ,rhs) (let ([v-rhs (val-of-cbv rhs env)]) (set-box! (apply-env env y) v-rhs))] 
      [`(,rator ,rand) (apply-closure (val-of-cbv rator env)
                                      (box (val-of-cbv rand env)))])))

(define closure-cbv
  (lambda (x body env)
    (lambda (a) (val-of-cbv body (extend-env x a env)))))




(define val-of-cbr
  (lambda (exp env)
    (match exp
      [`,b #:when (boolean? b) b]
      [`,n #:when (number? n)  n]
      [`,y #:when (symbol? y) (unbox (apply-env env y))]
      [`(zero? ,n) (zero? (val-of-cbr n env))]
      [`(sub1 ,n) (sub1 (val-of-cbr n env))]
      [`(* ,n1 ,n2) (* (val-of-cbr n1 env) (val-of-cbr n2 env))]
      [`(if ,test ,conseq ,alt) (if (val-of-cbr test env)
                                  (val-of-cbr conseq env)
                                  (val-of-cbr alt env))]
      [`(begin2 ,e1 ,e2) (begin (val-of-cbr e1 env) (val-of-cbr e2 env))]
      [`(random ,n) (random (val-of-cbr n env))]
      [`(lambda (,x) ,body) (closure-cbr x body env)]
      [`(,rator ,y) #:when(symbol? y) (apply-closure (val-of-cbr rator env) (apply-env env y))]
      [`(set! ,y ,rhs) (let ([v-rhs (val-of-cbr rhs env)]) (set-box! (apply-env env y) (val-of-cbr v-rhs env)))] 
      [`(,rator ,rand) (apply-closure (val-of-cbr rator env)
                                      (box (val-of-cbr rand env)))])))

(define closure-cbr
  (lambda (x body env)
    (lambda (a) (val-of-cbr body (extend-env x a env)))))


(define val-of-cbname
  (lambda (exp env)
    (match exp
      [`,b #:when (boolean? b) b]
      [`,n #:when (number? n)  n]
      [`,y #:when (symbol? y) ((unbox (apply-env env y)))]
      [`(zero? ,n) (zero? (val-of-cbname n env))]
      [`(sub1 ,n) (sub1 (val-of-cbname n env))]
      [`(* ,n1 ,n2) (* (val-of-cbname n1 env) (val-of-cbname n2 env))]
      [`(if ,test ,conseq ,alt) (if (val-of-cbname test env) (val-of-cbname conseq env) (val-of-cbname alt env))]
      [`(random ,n) (random (val-of-cbname n env))]
      [`(lambda (,x) ,body) (closure-cbname x body env)]
      [`(,rator ,y) #:when(symbol? y) (apply-closure (val-of-cbname rator env) (apply-env env y))]
      [`(,rator ,rand) (apply-closure (val-of-cbname rator env)
                                      (box (lambda()(val-of-cbname rand env))))])))

(define closure-cbname
  (lambda (x body env)
    (lambda (a) (val-of-cbname body (extend-env x a env)))))


(define val-of-cbneed
  (lambda (exp env)
    (match exp
      [`,b #:when (boolean? b) b]
      [`,n #:when (number? n)  n]
      [`,y #:when (symbol? y) (let ([b (apply-env env y)])
                                (let ([v ((unbox b))])
                                  (begin (set-box! b (lambda () v)) v)))]
      [`(zero? ,n) (zero? (val-of-cbneed n env))]
      [`(sub1 ,n) (sub1 (val-of-cbneed n env))]
      [`(* ,n1 ,n2) (* (val-of-cbneed n1 env) (val-of-cbneed n2 env))]
      [`(if ,test ,conseq ,alt) (if (val-of-cbneed test env) (val-of-cbneed conseq env) (val-of-cbneed alt env))]
      [`(random ,n) (random (val-of-cbneed n env))]
      [`(lambda (,x) ,body) (closure-cbneed x body env)]
      [`(,rator ,y) #:when(symbol? y) (apply-closure (val-of-cbneed rator env) (apply-env env y))]
      [`(,rator ,rand) (apply-closure (val-of-cbneed rator env) (box (lambda()(val-of-cbneed rand env))))])))

(define closure-cbneed
  (lambda (x body env)
    (lambda (a) (val-of-cbneed body (extend-env x a env)))))