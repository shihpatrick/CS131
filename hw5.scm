#lang racket
(define (null-ld? obj)
  (if (or (null? obj) (not (pair? obj)))
    #f
    (eq? (car obj) (cdr obj))))

(define (listdiff? obj)
  (cond
    ((null-ld? obj) #t)
    ((not (pair? obj)) #f)
    (else (let checkcar? ((ldcar (car obj)))
      (cond
 	((null? ldcar) (null? (cdr obj)))
	((not (pair? ldcar)) #f)
        ((eq? ldcar (cdr obj)) #t) 
	(else (checkcar? (cdr ldcar))))))))

(define (cons-ld obj listdiff)
  (cons (cons (car listdiff) obj) (cdr listdiff)))

(define (car-ld listdiff)
  (if (and (not(null-ld? listdiff)) (listdiff? listdiff))
    (car (car listdiff))
    "error"))

(define (cdr-ld listdiff)
  (if (and (not (null-ld? listdiff)) (listdiff? listdiff))
    (cons (cdr (car listdiff)) (cdr listdiff))
  "error"))

(define (listdiff obj . args)
  (cons (cons obj args) '())
)

(define (length-ld listdiff)
  (if (not (listdiff? listdiff))
    "error" 
    (let length-car ((ldcar (car listdiff)))
      (if (eq? ldcar (cdr listdiff)) 
        0
        (+ 1 (length-car (cdr ldcar)))))))
     
(define (append-ld ld . args)
  (if (null? args)
    ld 
   (let add-args ([ldargs (cons ld args)]) 
      (if (null? (cdr ldargs))
        (car ldargs)
        (cons-ld (listdiff->list (car ldargs)) (add-args (cdr ldargs)))))))

(define (assq-ld obj alistdiff)
  (let match-obj ([ldcar (car alistdiff)])
    (cond 
      ([null? ldcar] #f)
      ([eq? obj (car (car ldcar))] (car ldcar))   
      (else (match-obj (cdr ldcar))))))


(define (list->listdiff l)
  (cond
    ((null? l) "error")
    (else (cons l '()))))

(define (listdiff->list ld)
  (cond
    ((null-ld? ld) null)
    ((not (listdiff? ld)) "error")
    (else (let consume-car ((ldcar (car ld)))
      (if (eq? (cdr ld) ldcar)
        null
        (cons (car ldcar) (consume-car (cdr ldcar))))))))

(define (expr-returning ld)
  (define (get-lhs l)
    (if (null? l)
      `rhs
      `(cons ',(car l) ,(get-lhs (cdr l)))))
  (if (null-ld? ld)
    `(cons '() '())
    `(let ((rhs ',(cdr ld)))
       (cons ,(get-lhs(listdiff->list ld)) rhs))))

