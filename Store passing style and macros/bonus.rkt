#lang racket

;;1
(define filter
  (lambda (pred lst)
    (cond
      [(null? lst) '()]
      [ (pred (car lst)) (cons (car lst) (filter pred (cdr lst))) ]
      [else (filter pred (cdr lst)) ])))

(define filter-sps
  (lambda (pred lst store)
    (cond
      [(null? lst) (values '() store)]
      [ (pred (car lst))
        (let-values (((filt store) (filter-sps pred (cdr lst) store)))
          (values (cons (car lst) filt) store))]
      [else
          (let-values (((filt store) (filter-sps pred (cdr lst) store)))
            (values filt  `(,(car lst) . ,store)))])))

;2 Phew..

(define filter*
  (lambda (f ls)
    (cond
      [(null? ls) '()]
      [(pair? (car ls))
       (cons (filter* f (car ls)) (filter* f (cdr ls)))]
      [(null? (car ls)) '()]
      [(f (car ls)) (cons (car ls) (filter* f (cdr ls)))]
      [else (filter* f (cdr ls))])))


(define filter*-sps
  (lambda (pred lst store)
    (cond
      [(null? lst) (values '() store)]
      [(pair? (car lst))
        (let*-values (( (filt1 store1) (filter*-sps pred (car lst) store))
                    ((filt2 store2) (filter*-sps pred (cdr lst) store) ))
          (values  (cons filt1 filt2) (cons store1 store2)))]
      [(null? lst) (values '() store)]
         
      [ (pred (car lst))
        (let-values (((filt store) (filter*-sps pred (cdr lst) store)))
          (values (cons (car lst) filt) store))]
      [else
          (let-values (((filt store) (filter*-sps pred (cdr lst) store)))
            (values filt  `(,(car lst) . ,store)))])))

;3

(define fib-sps
  (lambda (n store)
    (cond
      ((assv n store) => ;;Pass on the value returned by assv as a parameter pr
       (lambda (pr) (values (cdr pr) store)))
      ((zero? n) (values n `((,n . 0) . ,store))) 
      ((= 1 n) (values n `((,n . 1) . ,store)))
      (else
       (let*-values (( (u store) (fib-sps (sub1 (sub1 n)) store))
                     ((v store) (fib-sps (sub1 n) store)))
         (values (+ u v) `((,n . ,(+ u v)) . ,store)))))))



;4

(define-syntax and*
  (syntax-rules ()
    ((_) #t)
    ((_ e0) e0)
    ((_ e0 e1 ...)  (if (not e0) #f  (and* e1 ...)))))



;5 cons*

(define-syntax cons*
  (syntax-rules ()
    ((_) (error 'cons-* "Incorrect argument count to cons*"))
    ((_ e0) e0)
    ((_ e0 e1 ...) `(,e0 . ,(cons* e1 ...)))))


;5 macro-list


(define-syntax macro-list
  (syntax-rules ()
    ((_) '())
    ((_ e0) `(,e0))
    ((_ e0 e1 ...)  `(,e0 . ,(macro-list e1 ...)))))

;6 mcond

(define-syntax mcond
  (syntax-rules (else)
    ((_) (void))
    ((_ (e0)) (let ((var e0)) (if var var (void))))
    ((_ (else e0)) e0)
    ((_ (c0 e0) (c1 e1) ...) (let ((con c0)) (if con e0 (mcond (c1 e1) ...))))))



;7 macro-map

(define-syntax macro-map
  (syntax-rules ()
    ((_ pr '()) '())
    ((_ pr '(e0 e1 ...)) (cons (pr e0) (macro-map pr '(e1 ...))))))