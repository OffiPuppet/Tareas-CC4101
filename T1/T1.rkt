#lang play

;; Kevin Alexis Iturra Carreño
;; Primavera 2022-2

#| WHITEBOARD POLICY

La función sequest fue discutida en base al whiteboard policy con las siguientes personas:

- Valeria Franciscangeli
- Martín Garrido
- Vicente Gatica
- Kevin Iturra
- Rodrigo Iturrieta
- Joaquin Muñoz
- Josefa Muñoz
- Camilo Rosas

|#



#| PARTE A |#

#|
<TaskSchedule> ::= (task <string> <integer>) 
            | (parallel-task) <TaskSchedule> <TaskSchedule>)
            | (serial-task) <TaskSchedule> <TaskSchedule>)
|#
;; Tipo recursivo para representar TaskSchedule's
;; La BNF es así ya que cada tarea tiene su nombre y número.
;; Además, puede ser paralela o en serie sin olvidar que es recursiva,
;; de ahí viene el left y right-task, tratando de imitar los BinTree vistos en clases
(deftype TaskSchedule
  (task name duration)
  (parallel-task left-task right-task)
  (serial-task left-task right-task))



#| PARTE B |#
;; is-in :: TaskSchedule string -> bool
;; Retorna verdadero o falso dependiendo si una tarea está en una TaskSchedule identificada por su nombre
(define (is-in task-schedule this-task)
  (match task-schedule
    [(task name _) (equal? name this-task)]
    [(parallel-task left-one right-one) (or (is-in left-one this-task)
                                            (is-in right-one this-task))]
    [(serial-task left-one right-one) (or (is-in left-one this-task)
                                            (is-in right-one this-task))]))



#| PARTE C |#
;; length :: TaskSchedule -> Integer
;; Retorna la duración total de la TaskSchedule, esto no es lo mismo que la suma de todas las duraciones de las task que componen la TaskSchedule
(define (length task-schedule)
  (match task-schedule
    [(task _ duration) duration]
    [(parallel-task left-one right-one) (max (length left-one) (length right-one))];; cuando son || entonces es el máximo entre cada uno,
    ;; si alguno está compuesto por varios(en serie, pasa a la linea de abajo y los suma)
    [(serial-task left-one right-one) (+ (length left-one) (length right-one))]))



#| PARTE D |#
;; longest :: TaskSchedule -> cons string integer
;; Retorna (un par formado por) el nombre y duración de la tarea más larga de un TaskSchedules
(define (longest task-in-schedule)
  (match task-in-schedule
    [(task name duration) (cons name duration)] ;;si hay solo uno, retornamos su name y duration
    [(parallel-task left-one right-one) (cond
                                          [(> (cdr (longest right-one)) (cdr (longest left-one)))
                                           (longest right-one)]
                                          [else (longest left-one)])];;si el resto del derecho es mayor al izquierdo, entonces hacemos longest derecho
                                          ;;sino, hacemos longest del izquierdo
    [(serial-task left-one right-one) (cond
                                          [(> (cdr (longest right-one)) (cdr (longest left-one)))
                                           (longest right-one)]
                                          [else (longest left-one)])]));; lo mismo que arriba pero en serie



#| PARTE E |#


;; has-parallel? :: TaskSchedule -> bool
;; Checkea si el taskscheduler tiene algo en paralelo
(define (has-parallel-task? taskscheduler)
  (match taskscheduler
    [(task name dur) #f]
    [(parallel-task t1 t2) #t]
    [(serial-task t1 t2) (or (has-parallel-task? t1) (has-parallel-task? t2))]))



;; sequest :: TaskSchedule -> integer
;; Retorna el largo de la secuencia más larga de tareas individuales
(define (sequest scheduler)
  (match scheduler
    [(task name dur) 1]
    [(parallel-task t1 t2) (max (sequest t1) (sequest t2))]
    [(serial-task t1 t2) (cond
                   [(and (has-parallel-task? t1) (has-parallel-task? t2)) (max (sequest t1) (sequest t2)) ]
                   [(and (not (has-parallel-task? t1)) (has-parallel-task? t2)) (cond
                                                                                  [(> (sequest t1) (max (sequest t2) (sequest t1))) (sequest t1)]
                                                                                  [else (max (sequest t1) (sequest t2))])]
                   [(and (has-parallel-task? t1) (not (has-parallel-task? t2))) (cond
                                                                                  [(> (sequest t2) (max (sequest t2) (sequest t1))) (sequest t2)]
                                                                                  [else (max (sequest t1) (sequest t2))])]
                   [(and (not (has-parallel-task? t1)) (not (has-parallel-task? t2))) (+ (sequest t1) (sequest t2))])]))



#| PARTE F |#
;; end-time :: TaskSchedule string -> integer
;; Retorna el instante de tiempo en el que termina de ejecutarse una tarea (identificada por su nombre), de acuerdo a un TaskSchedule
(define (end-time task-schedule name)
  (if (not (is-in task-schedule name))
      (error "tarea no encontrada") ;;error que pide el enunciado que ocurre cuando una tarea no está en la TaskSchedule
      (match task-schedule ;; en caso de estar en la TaskSchedule, hacemos el match
        [(task _ duration) duration] ;; caso base, retornamos la duración
        [(parallel-task left-one right-one) (cond ;; cuando es || vemos si la parte izq está en la TaskSchedule con ayuda de is-in, si lo está
                                              ;;hacemos end-time de la parte izq manteniendo el name
                                              [(is-in left-one name) (end-time left-one name)]
                                              [else (end-time right-one name)])] ;; sino, hacemos end-time de la parte der con name
        [(serial-task left-one right-one) (cond ;; cuando es || vemos si la parte izq está en la TaskSchedule con ayuda de is-in, si lo está
                                              ;;hacemos end-time de la parte izq manteniendo el name
                                              [(is-in left-one name) (end-time left-one name)]
                                              [else (+ (length left-one)(end-time right-one name))];;sino, hacemos la suma entre length de la izq
                                              ;; y end-time de la parte der con name.
                                              )])))



#| PARTE G |#
;; fold :: (String Number -> A) (A A -> A) -> (TaskSchedule -> A)
;; Ayuda respecto a la firma: el tipo de retorno del fold debe ser (TaskSchedule -> A)
;; Captura el esquema de recursión asociado al datatype TaskSchedule
(define (fold f g h)
  (λ (my-task)
    (match my-task
      [(task name duration) (f name duration)]
      [(parallel-task left-one right-one) (g ((fold f g h) left-one) ((fold f g h) right-one))]
      [(serial-task left-one right-one) (h ((fold f g h) left-one) ((fold f g h) right-one))])))



#| PARTE H |#
;; is-in2 :: TaskSchedule string -> bool
;; Retorna verdadero o falso dependiendo si una tarea está en una TaskSchedule identificada por su nombre pero usando fold
(define is-in2
  (lambda (task)
    (fold (lambda (a b) (equal? task a))
          (lambda (j k) (or j k))
          (lambda (j k) (or j k)))))



;; length2 ::  TaskScheduler -> Integer
;; Retorna la duración total de la TaskSchedule, esto no es lo mismo que
;; la suma de todas las duraciones de las task que componen la TaskSchedule usando fold
(define length2
   (fold (lambda (a b) b)
         (lambda (j k) (max j k))
         (lambda (j k) (+ j k))))



;; longest2 :: TaskSchedule -> cons string integer
;; Retorna (un par formado por) el nombre y duración de la tarea más larga de un TaskSchedule usando fold
(define longest2
   (fold (lambda (a b)
           (cons a b)) (lambda (j k)
                         (cond
                           [(> (cdr j) (cdr k)) j]
                           [else k]))
         (lambda (j k)
           (cond
             [(> (cdr j) (cdr k)) j]
             [else k]))))
