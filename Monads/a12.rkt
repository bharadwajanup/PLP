#lang racket
(require "monads.rkt")


;1
#|(define assv-maybe
  (lambda (x ls)
    (let ((res (assv x ls)))
    (cond
      [(not res) (fail)]
      [else (return-maybe (cdr res))]))))|#

(define assv-maybe
  (lambda(x ls)
    (cond
      [(null? ls) (fail)]
      [(equal? (caar ls) x) (bind-maybe (return-maybe (cdr (car ls))) (lambda(res)
                                                                        (return-maybe res)))]
      [else  (assv-maybe x (cdr ls))])))
                                                  


;2

(define partition-writer
  (lambda(pr ls)
    (cond
      [(null? ls) (return-writer '())]
      [(pr (car ls)) (bind-writer (tell-writer (car ls)) (lambda(_)
                                                           (partition-writer pr (cdr ls))))]
      [else (bind-writer (return-writer (car ls)) (lambda(a)
                                                    (let ((acc (partition-writer pr (cdr ls))))
                                                      `(,(cons a (car acc)) . ,(cdr acc)))))]
                                                    )))

;3

(define power
  (lambda (x n)
    (cond
      [(zero? n) 1]
      [(= n 1) x]
      [(odd? n) (* x (power x (sub1 n)))]
      [(even? n) (let ((nhalf (/ n 2)))
                   (let ((y (power x nhalf)))
                     (* y y)))])))


(define powerXpartials
  (lambda (x n)
    (cond
      [(zero? n) (return-writer 1)]
      [(= n 1) (return-writer x)]
      [(odd? n) 
       (bind-writer (powerXpartials x (- n 1))
                    (lambda (x^) 
                      (bind-writer (tell-writer x^)
                                   (lambda (_)
                                     (return-writer (* x x^)))))
                    
                    )]
      [(even? n)
       (bind-writer (powerXpartials x (/ n 2))
                    (lambda (x^)  
                      (bind-writer (tell-writer x^)
                                   (lambda (_)
                                     (return-writer (* x^ x^)))))
                    
                    )])))



;4
(define rwc
  (lambda (x ls acc)
    (cond
      [(null? ls) '()]
      [(pair? (car ls)) (cons (rwc x (car ls) acc) (rwc x (cdr ls) acc))]
      [(equal? (car ls) x) (cons acc (rwc x (cdr ls) (add1 acc)))]
      [else (cons (car ls) (rwc x (cdr ls) acc))])))



(define replace-with-count
  (lambda (x ls)
    (cond
      [(null? ls) (return-state ls)]

      [(pair? (car ls)) (bind-state (replace-with-count x (car ls)) (lambda(car_res)
                                                                      (bind-state (replace-with-count x (cdr ls)) (lambda(cdr_res)
                                                                                                                    (return-state (cons car_res cdr_res))))))]

      [(equal? (car ls) x) (bind-state get-state (lambda(st)
                                                   (bind-state (put-state (add1 st)) (lambda(_)
                                                                                      (bind-state (replace-with-count x (cdr ls)) (lambda (res)
                                                                                                                                    (return-state (cons st res))))))))]
      [else (bind-state (replace-with-count x (cdr ls)) (lambda(res)
                                                          (return-state (cons (car ls) res))))])))

(define traverse
    (lambda (return bind f)
      (letrec
        ((trav
           (lambda (tree)
             (cond
               [(pair? tree)
                (do bind
                  (a <- (trav (car tree)))
                  (d <- (trav (cdr tree)))
                  (return (cons a d)))]
               [else (f tree)]))))
        trav)))

;5
(define reciprocal
  (lambda(n)
    (cond
      [(equal? n 0) (fail)]
      [else (return-maybe (/ 1 n))])))



(define traverse-reciprocal
    (traverse return-maybe bind-maybe reciprocal))


;6
(define halve
  (lambda(n)
    (cond
      [(even? n) (return-writer (/ n 2))]
      [else (bind-writer (tell-writer n) (lambda(_)
                                           (return-writer n)))])))


(define traverse-halve
    (traverse return-writer bind-writer halve))


;7
(define state/sum
  (lambda(n)
    (bind-state get-state (lambda(st)
                            (bind-state (put-state (+ st n)) (lambda(_)
                                                              (return-state st)))))))

(define traverse-state/sum
    (traverse return-state bind-state state/sum))



;BrainTeaser


(define value-of
  (lambda (expr env)
    (match expr
      [(? number?) expr]
      [(? boolean?) expr]      
      [(? symbol?) (apply-env env expr)]
      [`(* ,x1 ,x2) (* (value-of x1 env) (value-of x2 env))]
      [`(sub1 ,x) (sub1 (value-of x env))]
      [`(zero? ,x) (zero? (value-of x env))]
      [`(if ,test ,conseq ,alt) (if (value-of test env)
                                   (value-of conseq env)
                                   (value-of alt env))]
      [`(capture ,k-id ,body) (call/cc (lambda (k)
                                         (value-of body (extend-env k-id k env))))]
      [`(return ,k-exp ,v-exp) ((value-of k-exp env) (value-of v-exp env))]
      [`(lambda (,id) ,body) (closure id body env)]
      [`(,rator ,rand) (apply-proc (value-of rator env) (value-of rand env))])))




(define value-of-cps
  (lambda(expr env)
    (match expr
      [(? number?) (return-cont expr)]
      [(? boolean?) (return-cont expr)]
      [(? symbol?) (return-cont (apply-env env expr))]
      [`(* ,x1 ,x2) (bind-cont (value-of-cps x1 env) (lambda(r1)
                                                       (bind-cont (value-of-cps x2 env) (lambda(r2)
                                                                                          (return-cont (* r1 r2))))))]
      [`(sub1 ,x) (bind-cont (value-of-cps x env) (lambda(r)
                                                    (return-cont (sub1 r))))]
      [`(zero? ,x) (bind-cont (value-of-cps x env) (lambda(r)
                                                    (return-cont (zero? r))))]
      [`(if ,test ,conseq ,alt) (bind-cont (value-of-cps test env) (lambda(r)
                                                                     (if r (value-of-cps conseq env) (value-of-cps alt env))))]                 
                                                                      
      [`(capture ,k-id ,body) (callcc (lambda (k)
                                         (value-of-cps body (extend-env k-id k env))))]
      
     [`(return ,k-exp ,v-exp) (bind-cont (value-of-cps k-exp env) (lambda(r1)
                                                                     (bind-cont (value-of-cps v-exp env) (lambda(r2)
                                                                                                           (r1 r2)))))]
      [`(lambda (,id) ,body) (return-cont (closure-cps id body env))]

      [`(,rator ,rand) (bind-cont (value-of-cps rator env) (lambda(rat)
                                                             (bind-cont (value-of-cps rand env) (lambda(ran)
                                                                                                  (apply-proc rat ran)))))])))
      
      
      






      
(define empty-env
  (lambda()
    (lambda (y) (error "unbound variable ~s" y))))

(define apply-env
  (lambda (env y)
    (env y)))

(define extend-env
  (lambda (x a env)
    (lambda(y) (if (eqv? x y) a (apply-env env y)))))

(define apply-proc
  (lambda(rator rand)
    (rator rand)))

(define closure
  (lambda (x body env)
    (lambda (a) (value-of body (extend-env x a env)))))

(define closure-cps
  (lambda (x body env)
    (lambda (a) (value-of-cps body (extend-env x a env)))))


(define fact-5
    '((lambda (f)
        ((f f) 5))
      (lambda (f)
        (lambda (n)
          (if (zero? n)
            1
            (* n ((f f) (sub1 n))))))))

(define capture-fun
    '(* 3 (capture q (* 2 (return q 4)))))












      




