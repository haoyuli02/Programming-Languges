#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

; 1
(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))

; 2
(define (string-append-map xs suffix)
  (map (lambda (str) (string-append str suffix)) xs))

; 3
(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n (length xs))))]))

; 4
(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (let ([next_pair (s)])
        (cons (car next_pair) (stream-for-n-steps (cdr next_pair) (- n 1))))))

; 5
(define funny-number-stream
  (letrec ([f (lambda (x) (if (= 0 (remainder x 5))
                              (cons (- 0 x) (lambda () (f (+ x 1))))
                              (cons x (lambda () (f (+ x 1))))))])
    (lambda () (f 1))))

; 6
(define dan-then-dog
  (letrec ([dan "dan.jpg"]
           [dog "dog.jpg"]
           [f (lambda (x) (if (= 0 (remainder x 2))
                              (cons dan (lambda () (f (+ x 1))))
                              (cons dog (lambda () (f (+ x 1))))))])
    (lambda () (f 0))))

; 7
(define (stream-add-zero s)
  (lambda () (let ([next_pair (s)])
               (cons (cons 0 (car next_pair)) (stream-add-zero (cdr next_pair))))))

; 8
(define (cycle-lists xs ys)
  (letrec ([f (lambda (x)
                (cons (cons (list-nth-mod xs x) (list-nth-mod ys x))
                      (lambda () (f (+ x 1)))))])
  (lambda () (f 0))))

; 9
(define (vector-assoc v vec)
  (letrec ([vec_size (vector-length vec)]
           [f (lambda (x) (if (>= x vec_size)
                           #f
                           (let ([curr (vector-ref vec x)])
                             (if (and (pair? curr) (equal? (car curr) v))
                                 curr
                                 (f (+ x 1))))))])
    (f 0)))

; 10
; This is a very important concept. You can only do this kind of momoization by returning a
; "memoized function". Notice that when you return the function, the function got defined has
; the memo in its closure. But if you want to write a function that can directly memoize things,
; you have to make the memo table globla or passed it as argument
(define (cached-assoc xs n)
  (let ([memo (make-vector n #f)]
        [next_pos 0])
  (lambda (v)
      (let ([check_memo (vector-assoc v memo)])
        (if check_memo
            check_memo
            (let ([result (assoc v xs)])
              (begin (vector-set! memo next_pos result) (if (= next_pos (- n 1))
                                                            (set! next_pos 0)
                                                            (set! next_pos (+ next_pos 1))) result)))))))

; Chanllenge Problem
(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
     (let ([first_arg e1])
       (letrec ([loop (lambda ()
                        (if (>= e2 first_arg)
                            #t
                            (loop)))])
         (loop)))]))




  
                     