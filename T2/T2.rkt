#lang play

#| ====== KEVIN ALEXIS ITURRA CARREÑO ====== |#


#| ==============================
            EJERCICIO 1
   ============================== |#

#| PARTE A |#

#|
Cond ::=
    | < <sym> <num>
    | > <sym> <num>
    | = <sym> <num>
    | & Cond Cond
    | orb Cond Cond
|#
;; Tipo inductivo para representar filtros WHERE del lenguaje Cmd 
(deftype Cond
  (< sym num)
  (> sym num)
  (= sym num)
  (& l-cond r-cond)
  (orb l-cond r-cond))

#|
Cmd ::=
    CREATE <sym> (listOf sym) Cmd
    INSERT (listOf num) <sym> Cmd
    FROM <sym> SELECT regs WHERE Cond
|#
;; Tipo inductivo para representar  el lenguaje de consulta Cmd
;; que nos permite crear tablas, ingresar registros
;; y finalmente consultar respecto a los registros
(deftype Cmd
  (CREATE name column cmd)
  (INSERT row name cmd)
  (FROM name SELECT regs WHERE cond))



#| PARTE B |#

#|
s-Cond ::= (list '= <sym> <num>)
           (list '< <sym> <num>)
           (list '> <sym> <num>)
           (list '& <s-Cond> <s-Cond>)
           (list 'or <s-Cond> <s-Cond>)
|#

;; parse-Cond :: s-Cond -> Cond
;; s-conds used as concrete 
;; syntax for our programs
;; convierte s-Cond a Cond
(define (parse-Cond s-cond)
  (match s-cond
    [(list '< l r) (< l r)]
    [(list '> l r) (> l r)]
    [(list '= l r) (= l r)]
    [(list '& l r) (& (parse-Cond l) (parse-Cond r))]
    [(list 'orb l r) (orb (parse-Cond l) (parse-Cond r))]))



#|
s-Cmd ::= (list 'CREATE <sym> (list sym) s-Cmd)
          (list 'INSERT (list num) <sym> s-Cmd)
          (list 'FROM <sym> 'SELECT 'regs 'WHERE s-Cond)
|#
;; parse :: s-Cmd -> Cmd
;; s-Cmds used as concrete 
;; syntax for our programs
;; convierte s-Cmd a Cmd
(define (parse s-cmd)
  (match s-cmd
    [(list 'CREATE table-name column cmd) (CREATE table-name column (parse cmd))]
    [(list 'INSERT row table-name cmd) (INSERT row table-name (parse cmd))]
    [(list 'FROM table-name 'SELECT 'regs 'WHERE cond) (FROM table-name 'SELECT 'regs 'WHERE (parse-Cond cond))])) ;;al final se usa parse-Cond para componer los parse


#| ==============================
            EJERCICIO 2
   ============================== |#

#| PARTE A |#

;; check-table :: Cmd -> Boolean / Error
;; comprueba que las tablas donde se insertan o consultan regristros se encuentren previamente definidas
(define (check-table cmd)
  (match cmd
    [(CREATE table-name column next-cmd) (check-table-with-list next-cmd (list table-name))] ;; llamamos a otra función sobre la siguiente cmd y la lista nueva
    [(INSERT row table-name next-cmd) (error 'Error\ Estatico\ A1 "Registro ~a insertado en tabla indefinida ~a." row table-name)] ;; Tirar error 1
    [(FROM table-name 'SELECT 'regs 'WHERE cond) (error 'Error\ Estatico\ A2 "La tabla consultada ~a no se encuentra definida." table-name)];; Tirar error 2
     ))



;; check-table-with-list :: Cmd (list sym) -> Boolean / Error
;; Checks if a table exists on the cmd.
(define (check-table-with-list cmd l)
  (match cmd
    [(CREATE table-name column next-cmd) (check-table-with-list next-cmd (append l (list table-name)))]
    [(INSERT row table-name next-cmd) (cond
                                        [(name-in-list l table-name) (check-table-with-list next-cmd l)]
                                        [else (error 'Error\ Estatico\ A1 "Registro ~a insertado en tabla indefinida ~a." row table-name)])]
    [(FROM table-name 'SELECT 'regs 'WHERE condition) (cond
                                                        [(name-in-list l table-name) #t]
                                                        [else (error 'Error\ Estatico\ A2 "La tabla consultada ~a no se encuentra definida." table-name)])]
  ))



;; name-in-list :: list <sym> -> Boolean
;; Checkea si un nombre(sym) está en una lista.
(define (name-in-list list name)
 (match list
    [(list) #f]
    [(cons left right) (cond
                          [(eq? name left) #t]
                          [else (name-in-list right name)])]))



#| PARTE B |#

;; Environment sacado de las clases
(deftype Env
  (mtEnv)
  (aEnv id val env))

(define empty-env (mtEnv))
 
(define extend-env aEnv)
 
(define (env-lookup x env)
  (match env
    [(mtEnv) #f]
    [(aEnv id val rest) (if (eq? id x)
                            val
                            (env-lookup x rest))]))



;; check-arity-with-env :: Cmd env -> Boolean / Error
;; Checkea que el arity del registro insertado sea igual que el arity del env
(define (check-arity-with-env cmd env)
  (match cmd
    [(CREATE table-name column next-cmd) (check-arity-with-env next-cmd (extend-env table-name (length column) env))]
    [(INSERT row table-name next-cmd) (cond
                                        [(equal? (env-lookup table-name env) (length row)) (check-arity-with-env next-cmd env)]
                                        [else (error 'Error\ Estatico\ B "Registro ~a con aridad ~a insertado en tabla ~a de aridad ~a." row (length row) table-name (env-lookup table-name env))])]
    [(FROM table-name 'SELECT 'regs 'WHERE cond) #t]))



;; check-arity :: Cmd -> Boolean / Error
;; Comprueba que al insertar registros, la aridad sea consistente en la tabla
(define (check-arity cmd)
  (match cmd
    [(CREATE table-name column next-cmd) (check-arity-with-env next-cmd (extend-env table-name (length column) empty-env))]
    [else (check-table cmd)]
  ))



#| PARTE C |#

;; check-column-cond-with-env :: Cond <sym> Env -> Boolean / Error
;; Checkea si las columnas en la condición existen en el env
(define (check-column-cond-with-env Cond table-name env)
  (match Cond
    [(or (< name num) (> name num) (= name num)) (cond
                                                   [(member name (env-lookup table-name env)) #t]
                                                   [else (error 'Error\ Estático\ C "La columna ~a no está definida en la tabla ~a." name table-name)])]
    [(or (orb lc rc) (& lc rc)) (check-column-cond-with-env lc table-name env) (check-column-cond-with-env rc table-name env)]))



;; check-column-with-env :: Cmd Env -> Boolean / Error
;; Checkea si las columnas referidas en el registro están presentes en el env
(define (check-column-with-env cmd env)
  (match cmd
    [(CREATE table-name column next-cmd) (check-column-with-env next-cmd (extend-env table-name column env))]
    [(INSERT row table-name next-cmd) (check-column-with-env next-cmd env)]
    [(FROM table-name 'SELECT 'regs 'WHERE cond) (check-column-cond-with-env cond table-name env)]
  ))



;; check-column :: Cmd -> Boolean / Error
;; verifica que los nombres consultados sean consistentes
(define (check-column cmd)
  (match cmd
    [(CREATE table-name column next-cmd) (def new-env (extend-env table-name column empty-env))
                              (check-column-with-env next-cmd new-env)]
    [else (check-table cmd)] ;; This matches the part A error.
  ))



#| PARTE D |#
;; static-check :: Cmd -> Boolean / Error
;; realiza los checking estáticos check-table, check-arity y check-column
(define (static-check Cmd)
  (check-table Cmd)
  (check-arity Cmd)
  (check-column Cmd))



#| ==============================
            EJERCICIO 3
   ============================== |#

#| PARTE A |#

;; interp-cond :: Cond Env -> List[Reg]
;; interpreta las condiciones Cond
(define (interp-cond Cond env)
  (match Cond
    [(= name num) (define position (let rep ((lst (env-lookup 'column env)) (id 0))
                               (cond
                                 [(empty? lst) #f]
                                 [(equal? (first lst) name) id]
                                 [else (rep (rest lst) (add1 id))]))) ;; el define retorna el indice del elemento seleccionado en una lista
                  (filter (lambda (e) (equal? (list-ref e position) num)) (env-lookup 'regs env)) ;; filtramos con lambda(e), y la función del equal? si el elemento position de la lista e es igual a num
                  ;;en el "objeto" (env-lookup 'regs env)
                  ]
    [(> name num) (define position (let rep ((lst (env-lookup 'column env)) (id 0))
                               (cond
                                 [(empty? lst) #f]
                                 [(equal? (first lst) name) id]
                                 [else (rep (rest lst) (add1 id))]))) (filter (lambda (e) (>= (list-ref e position) num)) (env-lookup 'regs env))]
    ;;se repite lo del caso = para >,<
    [(< name num) (define position (let rep ((lst (env-lookup 'column env)) (id 0))
                               (cond
                                 [(empty? lst) #f]
                                 [(equal? (first lst) name) id]
                                 [else (rep (rest lst) (add1 id))]))) (filter (lambda (e) (<= (list-ref e position) num)) (env-lookup 'regs env))]
    [(& left-cond right-cond) (set-intersect (interp-cond left-cond env) (interp-cond right-cond env))] ;; set-intersect(porque es un y lógico)
    ;;produce un set del mismo tipo que (interp-cond left-cond env) e incluye a (interp-cond right-cond env)
    [(orb left-cond right-cond) (set-union (interp-cond left-cond env) (interp-cond right-cond env))])) ;; set-union(porque es un o lógico)
    ;;produce un set del mismo tipo que (interp-cond left-cond env) e incluye a (interp-cond right-cond env)



;; interp-cmd :: Cmd Env -> List[Reg]
;; interpreta las instrucciones de un programa en Cmd
(define (interp-cmd cmd env)
  (match cmd
    [(CREATE table-name column cmd) (interp-cmd cmd (extend-env 'regs (list ) (extend-env 'table-name table-name (extend-env 'column column env))))]
    [(INSERT row table-name cmd)
     (define new-env (extend-env 'table-name (env-lookup 'table-name env) (extend-env 'column (env-lookup 'column env)
                                                                                      (extend-env 'regs (append (env-lookup 'regs env) (list row))
                                                                                                  empty-env))))
     (interp-cmd cmd new-env)]
    [(FROM table-name 'SELECT 'regs 'WHERE cond) (interp-cond cond env)]
    ))



#| PARTE B |#

;; run :: <s-Cmd> -> List[Reg] / Error
;; realiza un análisis estático del programa y en caso de no existir errores, lo ejecuta
(define (run cmd)
  (static-check cmd)
  (interp-cmd cmd empty-env))