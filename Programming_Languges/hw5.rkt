;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs but /is/ a MUPL value; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1

;; (a)
(define (racketlist->mupllist rac_list)
  (if (null? rac_list)
      (aunit)
      (apair (car rac_list) (racketlist->mupllist (cdr rac_list)))))

;; (b)
(define (mupllist->racketlist mupl_list)
  (if (aunit? mupl_list)
      null
      (cons (apair-e1 mupl_list) (mupllist->racketlist (apair-e2 mupl_list)))))

;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        [(int? e) e]
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               (error "MUPL ifgreater conditioned to non-number")))]
        [(aunit? e) e]
        [(isaunit? e) (if (aunit? (eval-under-env (isaunit-e e) env)) (int 1) (int 0))]
        [(closure? e) e]
        [(apair? e) (apair (eval-under-env (apair-e1 e) env) (eval-under-env (apair-e2 e) env))]
        [(fst? e)
         (let ([v (eval-under-env (fst-e e) env)])
           (if (apair? v)
               (apair-e1 v)
               (error "MUPL fst applied to non-pair")))]
        [(snd? e)
         (let ([v (eval-under-env (snd-e e) env)])
           (if (apair? v)
               (apair-e2 v)
               (error "MUPL snd applied to non-pair")))]
        [(fun? e) (closure env e)]
        [(mlet? e)
         (let ([name (eval-under-env (mlet-e e) env)])
           (eval-under-env (mlet-body e) (cons (cons (mlet-var e) name) env)))]
        [(call? e) ; (struct call (funexp actual) 
         (let ([function_closure (eval-under-env (call-funexp e) env)]
               [param (eval-under-env (call-actual e) env)])
           (if (closure? function_closure) ; (struct closure (env fun) #:transparent) 
               (let* ([actual_fun (closure-fun function_closure)] ; (struct fun  (nameopt formal body)
                      [function_body (fun-body actual_fun)]
                      [arg_name (fun-formal actual_fun)]
                      [fun_name (fun-nameopt actual_fun)]
                      [environment (closure-env function_closure)])
                  (if fun_name
                      (eval-under-env function_body (cons (cons arg_name param) (cons (cons fun_name function_closure) environment)))
                      (eval-under-env function_body (cons (cons arg_name param) environment))))
               (error "MUPL call applied to non-closure")))]
        [#t (error (format "bad MUPL expression: ~v" e))])) 

;; Do NOT change
(define (eval-exp e)
 (eval-under-env e null))
        
;; Problem 3

(define (ifaunit e1 e2 e3)
  (ifgreater (isaunit e1) (int 0) e2 e3))

(define (mlet* lstlst e2)
  (if (null? lstlst)
      e2
      (mlet (car (car lstlst)) (cdr (car lstlst)) (mlet* (cdr lstlst) e2))))

(define (ifeq e1 e2 e3 e4)
  (mlet* (list (cons "_x" e1) (cons "_y" e2))
         (ifgreater (var "_x") (var "_y") e4 (ifgreater (var "_y") (var "_x") e4 e3))))

;; Problem 4

(define mupl-map
  (fun #f "function"
           (fun "f" "mupl_list"
                    (ifaunit (var "mupl_list")
                             (aunit)
                             (apair (call (var "function") (fst (var "mupl_list")))
                                    (call (var "f") (snd (var "mupl_list"))))))))

(define mupl-mapAddN 
  (mlet "map" mupl-map
        (fun #f "mupl_int"
             (call (var "map") (fun #f "x" (add (var "x") (var "mupl_int")))))))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e)
  (cond [(var? e) e]
        [(int? e) e]
        [(add? e) (add (compute-free-vars (add-e1 e)) (compute-free-vars (add-e2 e)))]
        [(ifgreater? e) (ifgreater (compute-free-vars (ifgreater-e1 e)) (compute-free-vars (ifgreater-e2 e))
                                   (compute-free-vars (ifgreater-e3 e)) (compute-free-vars (ifgreater-e4 e)))]
        [(call? e) (call (compute-free-vars (call-funexp e)) (compute-free-vars (call-actual e)))]
        [(mlet? e) (mlet (mlet-var e) (compute-free-vars (mlet-e e)) (compute-free-vars (mlet-body e)))]
        [(apair? e) (apair (compute-free-vars (apair-e1 e)) (compute-free-vars (apair-e2 e)))]
        [(fst? e) (fst (compute-free-vars (fst-e e)))]
        [(snd? e) (snd (compute-free-vars (snd-e e)))]
        [(aunit? e) e]
        [(isaunit? e) (isaunit (compute-free-vars (isaunit-e e)))]
        [(fun? e)
         (letrec ([things_not_free (mutable-set)]
                  [compute_free (lambda (f)
                                  (cond [(var? f) (set (var-string f))]
                                        [(int? f) (set)]
                                        [(add? f) (set-union (compute_free (add-e1 f)) (compute_free (add-e2 f)))]
                                        [(ifgreater? f) (set-union (compute_free (ifgreater-e1 f)) (compute_free (ifgreater-e2 f))
                                                                   (compute_free (ifgreater-e3 f)) (compute_free (ifgreater-e4 f)))]
                                        [(call? f) (set-union (compute_free (call-funexp f)) (compute_free (call-actual f)))]
                                        [(mlet? f) (let ([temp (set-union (compute_free (mlet-e f)) (compute_free (mlet-body f)))])
                                                     (if (set-member? temp (mlet-var f))
                                                         (set-remove temp (mlet-var f))
                                                         temp))]
                                        [(apair? f) (set-union (compute_free (apair-e1 f)) (compute_free (apair-e2 f)))]
                                        [(fst? f) (compute_free (fst-e f))]
                                        [(snd? f) (compute_free (snd-e f))]
                                        [(aunit? f) (set)]
                                        [(isaunit? f) (compute_free (isaunit-e f))]
                                        [(fun? f) (begin (set-add! things_not_free (fun-nameopt f)) (set-add! things_not_free (fun-formal f)) (compute_free (fun-body f)))]
                                        [(closure? f) (compute_free (closure-fun f))]
                                        [#t (error "Invalid expressions")]))])
           (fun-challenge (fun-nameopt e) (fun-formal e) (compute-free-vars (fun-body e)) (set-subtract (compute_free e) things_not_free)))]
        [(closure? e) (closure (closure-env e) (compute-free-vars (closure-fun e)))]
        [#t (error "Invalid expressions")]))
           
                                             
;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env)
  (cond [(var? e) 
         (var-string e)]
        [(add? e) 
         (let ([v1 (eval-under-env-c (add-e1 e) env)]
               [v2 (eval-under-env-c (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        [(int? e) e]
        [(ifgreater? e)
         (let ([v1 (eval-under-env-c (ifgreater-e1 e) env)]
               [v2 (eval-under-env-c (ifgreater-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env-c (ifgreater-e3 e) env)
                   (eval-under-env-c (ifgreater-e4 e) env))
               (error "MUPL ifgreater conditioned to non-number")))]
        [(aunit? e) e]
        [(isaunit? e) (if (aunit? (eval-under-env-c (isaunit-e e) env)) (int 1) (int 0))]
        [(closure? e) e]
        [(apair? e) (apair (eval-under-env-c (apair-e1 e) env) (eval-under-env-c (apair-e2 e) env))]
        [(fst? e)
         (let ([v (eval-under-env-c (fst-e e) env)])
           (if (apair? v)
               (apair-e1 v)
               (error "MUPL fst applied to non-pair")))]
        [(snd? e)
         (let ([v (eval-under-env-c (snd-e e) env)])
           (if (apair? v)
               (apair-e2 v)
               (error "MUPL snd applied to non-pair")))]
        [(fun-challenge? e)
         (letrec ([used (fun-challenge-freevars e)]
                  [find_used_vars (lambda (env)
                                    (if (null? env)
                                        null
                                        (if (set-member? used (car env))
                                            (cons (car env) (find_used_vars (cdr env)))
                                            (find_used_vars (cdr env)))))])
           (closure (find_used_vars env) e))]
        [(mlet? e)
         (let ([name (eval-under-env-c (mlet-e e) env)])
           (eval-under-env-c (mlet-body e) (cons (cons (mlet-var e) name) env)))]
        [(call? e) ; (struct call (funexp actual) 
         (let ([function_closure (eval-under-env-c (call-funexp e) env)]
               [param (eval-under-env-c (call-actual e) env)])
           (if (closure? function_closure) ; (struct closure (env fun) #:transparent) 
               (let* ([actual_fun (closure-fun function_closure)] ; (struct fun  (nameopt formal body)
                      [function_body (fun-challenge-body actual_fun)]
                      [arg_name (fun-challenge-formal actual_fun)]
                      [fun_name (fun-challenge-nameopt actual_fun)]
                      [environment (closure-env function_closure)])
                  (if fun_name
                      (eval-under-env-c function_body (cons (cons arg_name param) (cons (cons fun_name function_closure) environment)))
                      (eval-under-env-c function_body (cons (cons arg_name param) environment))))
               (error "MUPL call applied to non-closure")))]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))

; (define eval-exp eval-exp-c)
