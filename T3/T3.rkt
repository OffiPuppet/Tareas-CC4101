#lang play

#|

   ========================================
                   Tarea 3
   ========================================
   Nombre: Kevin Alexis Iturra Carreño
   Rut: 20.532.318-K

|#


#|
<expr> ::= (num <num>)
         | (add <expr> <expr>)
         | (sub <expr> <expr>)
         | (if0 <expr> <expr> <expr>)
         | (with <sym> <expr> <expr>)
         | (id <id>)
         | (fun <sym> <expr>)
         | (app <expr> <expr>)
|#
;; deftype del Lenguaje de una expr
(deftype Expr
  (num n)
  (add l r)
  (sub l r)
  (if0 c t f)
  (with x ne b)
  (fun id body)
  (id s)
  (app fun-expr arg-expr))


;; --------- [ Parte 1 ] ---------

;;parse :: s-expr -> Expr

#| where
   <s-expr> ::= <num>
              | <sym>
              | (list '+ <s-expr> <s-expr>)
              | (list '- <s-expr> <s-expr>)
              | (list 'if0 <s-expr> <s-expr> <s-expr>)
              | (list 'with (list <sym> <s-expr>) <s-expr>)
|#
;; parsea una s-expr
(define (parse s-expr)
  (match s-expr
    [(? number?) (num s-expr)]
    [(? symbol?) (id s-expr)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]
    [(list 'if0 c t f) (if0 (parse c)
                            (parse t)
                            (parse f))]
    [(list 'with (list x e) b) (with x (parse e) (parse b))]
    [(list 'fun (list x) boby) (fun x (parse boby))]
    [(list fun-expr arg-expr) (app (parse fun-expr) (parse arg-expr))]))


;; --------- [ Parte 1.A ] ---------

;; const? :: Expr -> bool
;; indica si es que la expresión entregada consiste en sólo constantes numéricas, posiblemente combinadas 
;; a través de operadores aritméticos y/o condicionales y/o definiciones locales.
(define (const? expr)
  (match expr
    [(num n) #t]
    [(id _) #f]
    [(fun _ _) #f]
    [(app _ _) #f]
    [(add l r)(and (const? l) (const? r))]
    [(sub l r)(and (const? l) (const? r))]
    [(if0 c t f)(and (const? c)(const? t)(const? f))]
    [(with _ ne b)(and (const? ne)(const? b))])) ;; usamos "_", ya que no nos importa ese valor, solo el ne y b


;; --------- [ Parte 1.B ] ---------

;; Interface of the Abstract Dada Type (ADT) for  
;; keeping track of the deferred substitutions

;; empty-env  :: Env
;; extend-env :: Symbol Value Env -> Env
;; env-lookup :: Symbol Env -> Value

;; Implementation of the ADT

;; <env> ::= mtEnv
;;         | (aEnv <id> <value> <env>)
(deftype Env
  (mtEnv)
  (aEnv id val env))

(define empty-env (mtEnv))
 
(define extend-env aEnv)
 
(define (env-lookup x env)
  (match env
    [(mtEnv) (error 'env-lookup "free identifier: ~a" x)]
    [(aEnv id val rest) (if (symbol=? id x)
                            val
                            (env-lookup x rest))]))



;; calc :: Expr Env -> number
; usaremos la función calc definida en clases para ayudarnos en la construcción de fold-consts
;; pero le agregaremos env's como parámetro, lo que le deja casi la misma estructura definida en clases
;; solo cambia el with y id que usa lookup.
(define (calc expr env)
  (match expr
    [(num n) n]
    [(id x) (env-lookup x env)]
    [(add l r) (+ (calc l env) (calc r env))]
    [(sub l r) (- (calc l env) (calc r env))]
    [(if0 c t f) (if (zero? (calc c env))
                     (calc t env)
                     (calc f env))]
    [(with x ne b) (def new-env (extend-env x (calc ne env) env)) (calc b new-env)]))

;; fold-consts :: Expr -> Expr
;; fold-consts aplica constant folding recursivamente a una expresión.
(define (fold-consts expr)
  (match expr
    [(num n) (num n)]
    [(id expr) (id expr)]
    [(add l r) (cond
                 [(const? expr) (num (calc expr empty-env))]
                 [else (add (fold-consts l) (fold-consts r))])]
    [(sub l r) (cond
                 [(const? expr) (num (calc expr empty-env))]
                 [else (sub (fold-consts l) (fold-consts r))])]
    [(if0 c t f) (cond
                   [(const? expr) (num (calc expr empty-env))]
                   [else (if0 (fold-consts c)(fold-consts t)(fold-consts f))])]
    [(with x ne b) (cond
                     [(const? expr) (num (calc expr empty-env))]
                     [else (with x (fold-consts ne) b)])]
    [(fun x body) (fun x (fold-consts body))]
    [(app fun-expr arg-expr) (app fun-expr (fold-consts arg-expr))]))

;; --------- [ Parte 2 ] ---------

;; propagate-consts :: Expr -> Expr
;; aplica constant propagation recursivamente.
;; Su implementación deberá propagar las constantes bottom-up, es decir desde las expresiones más internas, hacia la más externas.
;; Para ello, ocupa una función auxiliar con envs, llamada propagate-consts-env.
(define (propagate-consts expr)
  (propagate-consts-env expr empty-env))

;; propagate consts :: Expr Env -> Expr
;; aplica constant propagation usando enviroments, manteniendo la propagacipon bottom-up.
(define (propagate-consts-env expr env)
  (match expr
    [(num n) (num n)]
    [(id x) (env-lookup x env)]
    [(add l r) (add (propagate-consts-env l env) (propagate-consts-env r env))]
    [(sub l r) (sub (propagate-consts-env l env) (propagate-consts-env r env))]
    [(if0 c t f) (if0 (propagate-consts-env c env) (propagate-consts-env t env) (propagate-consts-env f env))]
    [(with x ne b) (def new-env (extend-env x (cond
                                                [(const? ne) (num (calc (propagate-consts-env ne env) env))]
                                                [else (id x)]) env))
                   (cond
                     [(const? ne) (propagate-consts-env b new-env)]
                     [else (with x (propagate-consts-env ne new-env) (propagate-consts-env b new-env))])]
    [(fun x body) (def new-env (extend-env x (cond
                                                [(const? body) (num (calc (propagate-consts-env body env) env))]
                                                [else (propagate-consts-env body env)]) env))
                  (cond
                    [(const? body) (propagate-consts-env x new-env)]
                    [else (fun x (cond
                                   [(const? (propagate-consts-env body new-env))(propagate-consts-env body new-env)]
                                   [else body]))])]
    [(app fun-expr arg-expr) (app fun-expr arg-expr)]))



;; --------- [ Parte 3 ] ---------

;; cf&p :: Expr -> Expr
;; Función que aplica constant folding y propagation iterativamente hasta alcanzar el punto fijo.
(define (cf&p expr)
  (match* ((fold-consts expr) (propagate-consts (fold-consts expr)))
    [(left right) #:when (equal? left right) left]
    [(_ right) (cf&p right)]))
