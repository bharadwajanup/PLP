#lang racket
(require "mk.rkt")
(require "numbers.rkt")

;; Part I Write the answers to the following problems using your
;; knowledge of miniKanren.  For each problem, explain how miniKanren
;; arrived at the answer.  You will be graded on the quality of your
;; explanation; a full explanation will require several sentences.

;; 1 What is the value of 

(run 2 (q)
  (== 5 q)
  (conde
   [(conde 
     [(== 5 q)
      (== 6 q)])
    (== 5 q)]
   [(== q 5)]))

#|
run acts as an interface between racket and minikanren.
2 in run represents that we need two answers out of the expression and q is the query variable.

When a call to run is made, the output is always the possible answers for the query variables supplied. In this case, the output will be the possible values for q
(== 5 q) means minkanren will look for an answer in which q and 5 are equal. The only possibility in this case is q being 5.
"conde" is very similar to "cond" syntactically and is used to generate multiple answers. In this case, the first clause of outer conde fails ('()) because q cannot have a value where it is equal
 to both 5 and 6 ("(== 5 q) (== 6 q)")

Finally, minikanren would look for the value of q where it is equal to q. The only possible answer is 5.

Hence, the output of the program is '(5).

|#


;; 2 What is the value of
#|(run 1 (q) 
  (fresh (a b) 
    (== `(,a ,b) q)
    (absento 'tag q)
    (symbolo a)))|#
(run 1 (q) 
  (fresh (a b)
   ; (== a 'a)(== b 'tag) is a failure
    (== `(,a ,b) q)
    (absento 'tag q)
    (symbolo a))) ;; if we know that a is a symbol, the absento can be simplified to be a disequality constraint.

#|
In the above example, the run 1 means we would like to generate one answer from the expresstion where  q is our query variable.

We also have a fresh operation where variables a and b are introduced. With "(== `(,a ,b) q) minikanren will now look for values of q where q is a list of values of a and b.
we also have absento operation where it adds a constraint to the value of q such that q does not contain the symbol 'tag.
Using symbolo, another constraint is added such that a must be a symbol.

Now,  given that a must be a symbol and our q is a list of values of a and b such that it does not contain the symbol 'tag.

This could mean that the value q could have any value but a must be a symbol, b can be anything but neither can have the symbol 'tag in them. Having values of a and b which are non-compliant
to these rules will result in a failure continuation ('())

The result of the above expression now becomes:
'(((_.0 _.1) (=/= ((_.0 tag))) (sym _.0) (absento (tag _.1))))

Which means that both a and b could have any value represented by _.0 and _.1 respectively, there exists a disequal relation for a and 'tag and 'tag should not be a part of b.

One more thing to observe here is that since minikanren knows that a must be a symbol, it simplified the absento operation to be a disequality constraint between 'tag and a.
|#

;; 3 What do the following miniKanren constraints mean?
;; a == : Unifies two terms. (== 5 q) notifies that q is associated with the value 5.
;; b =/= : disequality constraint. (=/= q 5) notifies that q cannot be associated with the value 5.
;; c absento : A constraint which specifies that ensures a symbol does not appear in the term. (absento 'tag t) ensures that t dooes not contain 'tag. 
;; d numbero : Constraint which ensures that a term is a number. (numbero a) means that a now must be a number.
;; e symbolo : Constraint that ensures that a term is a symbol. (symbolo s) means that s must now be a symbol.

;; Part II goes here.

(define assoc
  (lambda (x ls)
    (match-let* ((`(,a . ,d) ls)
                 (`(,aa . ,da) a))
      (cond
        ((equal? aa x) a)
        ((not (equal? aa x)) (assoc x d))))))


;;mk assoco
(define (assoco x ls out)
   (fresh (a d aa da)
          (== `(,a . ,d) ls)
          (== `(,aa . ,da) a)
          (conde
           [(== aa x) (== out a)]
           [(=/= aa x) (assoco x d out)])))

(define reverse
  (lambda (ls)
    (cond
      ((equal? '() ls) '())
      (else
       (match-let* ((`(,a . ,d) ls)
                    (res (reverse d)))
         (append res `(,a)))))))

            
;;mk reverso
(define (reverseo ls out)
  (conde
   [(== '() ls) (== '() out)]
   [(fresh (a d res)
    (== `(,a . ,d) ls)
     (reverseo d res)
    (appendo res `(,a) out))]))


(define stutter
  (lambda (ls)
    (cond
      ((equal? '() ls) '())
      (else 
        (match-let* ((`(,a . ,d) ls)
		     (res (stutter d)))
          `(,a ,a . ,res))))))


;;mk stuttero
(define (stuttero ls out)
  (conde
   [(== '() ls) (== out '())]
   [(fresh (a d res)
           (== `(,a . ,d) ls)
           (==`(,a ,a . ,res) out)
           (stuttero d res)
           )]))




;;Brain Teaser.

(define length
  (lambda(ls)
    (cond
      [(equal? ls '()) 0]
      [else
       (match-let* ((`(,a . ,d) ls))
         (add1 (length d)))])))


(define (+o n m out)
  (addero 0 n m out))


(define (lengtho ls out)
  (conde
   [(== ls '()) (== out '())]
   [(fresh (a d res)
           (== `(,a . ,d) ls)
            (lengtho d res)
            (+o '(1) res out))]))



      