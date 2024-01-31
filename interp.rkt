#lang eopl

;; interpreter for the LET language.  The \commentboxes are the
;; latex code for inserting the rules into the code in the book.
;; These are too complicated to put here, see the text, sorry.

(require "lang.rkt")
(require "data-structures.rkt")
(require "environments.rkt")

(provide value-of-program value-of)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

;; value-of-program : Program -> ExpVal
;; Page: 71
(define value-of-program 
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
                 (value-of exp1 (init-env))))))

;; value-of : Exp * Env -> ExpVal
;; Page: 71
(define value-of
  (lambda (exp env)
    (cases expression exp
      
      (const-exp (num) (num-val num))

      (var-exp (var) (apply-env env var))
      
      (op-exp (exp1 exp2 op)
              (let ((val1 (value-of exp1 env))
                    (val2 (value-of exp2 env)))
                  (let ((num1 (expval->rational val1))
                        (num2 (expval->rational val2)))
                      (cond
                        ;; NUMBER * NUMBER
                        ((and (number? num1) (number? num2))
                          (num-val
                            (cond 
                              ((= op 1) (+ num1 num2))
                              ((= op 2) (* num1 num2))
                                    ;; -----------------------
                                    ;; INSERT YOUR CODE HERE 
                                    ;; -----------------------
                              ((= op 3) (/ num1 num2))
                              (else (- num1 num2))

                                    ;; -----------------------
                              )))
                        ;; NUMBER * RATIONAL
                        ((and (number? num1) (not (number? num2)))
                          (rational-val
                          (let ((num2top (car num2))
                                (num2bot (cdr num2)))
                            (cond 
                              ((= op 1) (cons (+ (* num1 num2bot) num2top) num2bot))
                              ((= op 2) (cons (* num1 num2top) num2bot))
                              ;; -----------------------
                              ;; INSERT YOUR CODE HERE 
                              ;; -----------------------
                              ((= op 3) (cons (* num1 num2bot) num2top))
                              (else (cons (- (* num1 num2bot) num2top) num2bot))

                              ;; -----------------------

                              
                              ))))
                        ;; RATIONAL * NUMBER
                        ((and (number? num2) (not (number? num1)))
                          (rational-val
                          (let ((num1top (car num1))
                                (num1bot (cdr num1)))
                            (cond 
                              ((= op 1) (cons (+ (* num1bot num2) num1top) num1bot))
                              ((= op 2) (cons (* num1top num2) num1bot))
                              ;; -----------------------
                              ;; INSERT YOUR CODE HERE 
                              ;; -----------------------
                              ((= op 3) (cons num1top (* num1bot num2)))
                              (else (cons (- num1top (* num1bot num2)) num1bot))

                              ;; -----------------------
                              ))))
                        ;; RATIONAL * RATIONAL
                        (else
                          (rational-val
                          (let ((num1top (car num1))
                                (num1bot (cdr num1))
                                (num2top (car num2))
                                (num2bot (cdr num2)))
                            (cond 
                              ((= op 1) (cons (+ (* num1top num2bot) (* num1bot num2top)) (* num1bot num2bot))) ;; add
                              ((= op 2) (cons (* num1top num2top) (* num1bot num2bot))) ;; multiply
                              ;; -----------------------
                              ;; INSERT YOUR CODE HERE 
                              ;; -----------------------
                              ((= op 3) (cons (* num1top num2bot) (* num1bot num2top)))
                              (else (cons (- (* num1top num2bot) (* num1bot num2top)) (* num1bot num2bot)))
                              ;; ----------------------- 
                            ))))))))
      (zero?-exp (exp1)
                 (let ((val1 (value-of exp1 env)))
                   (let ((num1 (expval->rational val1)))
                     (if (number? num1)
                        (if (zero? num1)
                          (bool-val #t)
                          (bool-val #f))
                          ;; -----------------------
                          ;; INSERT YOUR CODE HERE 
                          ;; -----------------------
                        (let ((num1top (car num1)))
                          (if (zero? num1top)
                              (bool-val #t)
                              (bool-val #f)))

                          ;; ----------------------- 
                        ))))

      

      (let-exp (var exp1 body)       
               (let ((val1 (value-of exp1 env)))
                 (value-of body
                           (extend-env var val1 env))))

      ;; -----------------------
      ;; INSERT YOUR CODE HERE 
      ;; -----------------------
      ;; TODO
      (if-exp (exp1 exp2 conds exps exp3)
              (let ((val1 (value-of exp1 env))
                    (val2 (value-of exp2 env))
                    (val3 (value-of exp3 env)))
                (let ((pred (expval->bool val1)))
                      (if pred val2
                          (value-of (if-helper conds exps exp3 env) env)))))
                    
        
      (list-exp ()
                (list-val '()))

      (cons-exp (exp1 exp2)
                (let ((val1 (value-of exp1 env))
                      (val2 (value-of exp2 env)))
                  (let ((num1 (expval->rational val1))
                        (lst2 (expval->list val2)))
                    (list-val (cons num1 lst2)))))

      (sum-exp (lst)
               (let ((val (value-of lst env)))
                 (let ((lst1 (expval->list val)))
                   (num-val (sum-lst lst1)))))

      (rational-exp (num1 num2)
                (if (zero? num2)
                    (eopl:error "Denominator cannot be 0")
                    (rational-val (cons num1 num2))))

      (simpl-exp (exp1)
                 (let ((val1 (value-of exp1 env)))
                   (let ((num1 (expval->rational val1)))
                     (cond
                       ((number? num1) num1)
                       ((pair? num1)
                        (let ((numerator (car num1))
                              (denominator (cdr num1)))
                          (let ((divider (gcd numerator denominator)))
                            (rational-val (cons (/ numerator divider) (/ denominator divider))))))))))
                                  
      ;; -----------------------

      )))

;; HELPER METHOD
(define (sum-lst lst)
   (if (null? lst)
       0
       (+ (car lst) (sum-lst (cdr lst)))))

(define (if-helper conds exps exp3 env)
  (cond
    ((null? conds) exp3)
    ((expval->bool (value-of (car conds) env)) (car exps))
    (else (if-helper (cdr conds) (cdr exps) exp3 env))))

; Euclidean GCD algorithm
(define (gcd num1 num2)
  (cond
    ((= num1 num2) num1)
    ((> num1 num2) (gcd (- num1 num2) num2))
    ((< num1 num2) (gcd (- num2 num1) num1))))
