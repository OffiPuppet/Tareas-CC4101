#lang play
(require "T1.rkt")

(print-only-errors #t)

#| PARTE A |#

;; Definición del caso de prueba
;; caso de prueba enunciado
(define my-taskschedule
  (serial-task
   (parallel-task
    (serial-task
     (task "t1" 2)
     (task "t2" 4))
    (task "t3" 3))
   (parallel-task
    (serial-task
     (task "t4" 2)
     (parallel-task
      (task "t6" 2)
      (task "t7" 1)))
    (task "t5" 6))))

(test (TaskSchedule? my-taskschedule) #t)

#| PARTE B |#

(test (is-in (task "t0" 1) "t0") #t)
(test (is-in (task "t1" 2) "t0") #f)
(test (is-in (parallel-task (task "t2" 3) (task "t3" 2)) "t2") #t)
(test (is-in (serial-task (task "t4" 4) (task "t5" 2)) "t2") #f)
(test (is-in my-taskschedule "t2") #t)
(test (is-in my-taskschedule "collect") #f)

#| PARTE C |#

(test (length (task "t0" 5)) 5)
(test (length (parallel-task (task "t1" 6) (task "t2" 5))) 6)
(test (length (parallel-task (task "t3" 6) (task "t4" 7))) 7)
(test (length (serial-task (task "t5" 7) (task "t6" 1))) 8)
(test (length (serial-task (task "t7" 1) (task "t8" 7))) 8)
(test (length my-taskschedule) 12)

#| PARTE D |#

(test (longest (task "t0" 3)) (cons "t0" 3))
(test (longest (parallel-task (task "t1" 1) (task "t2" 2))) (cons "t2" 2))
(test (longest (parallel-task (task "t3" 3) (task "t4" 1))) (cons "t3" 3))
(test (longest (serial-task (task "t5" 2) (task "t6" 1))) (cons "t5" 2))
(test (longest (serial-task (task "t7" 1) (task "t8" 2))) (cons "t8" 2))
(test (longest my-taskschedule) (cons "t5" 6))

#| PARTE E |#

(test (sequest (task "t0" 2)) 1)
(test (sequest (parallel-task (task "t1" 2) (task "t2" 1))) 1)
(test (sequest (parallel-task (serial-task (task "t3" 3) (task "t4" 4)) (task "t5" 5))) 2)
(test (sequest (parallel-task (serial-task (task "t6" 6) (task "t7" 7)) (serial-task (task "t8" 8) (serial-task (task "t9" 9) (task "t10" 10))))) 3)


(test (sequest (serial-task
                (parallel-task
                 (serial-task
                  (serial-task
                   (serial-task
                    (task "t7" 7)
                    (task "t8" 8))
                   (task "t9" 9))
                  (task "t10" 10))
                 (task "t6" 6))
                 (serial-task
                  (task "t4" 4)
                  (task "t5" 5)))) 4)

(test (sequest (parallel-task
                (parallel-task
                 (serial-task
                  (serial-task
                   (serial-task
                    (task "t7" 7)
                    (task "t8" 8))
                   (task "t9" 9))
                  (task "t10" 10))
                 (task "t6" 6))
                 (serial-task
                  (task "t4" 4)
                  (task "t5" 5)))) 4)

(test (sequest (serial-task
                (serial-task
                  (task "t4" 4)
                  (task "t5" 5))
                (parallel-task
                  (serial-task
                   (serial-task
                    (serial-task
                     (task "t7" 7)
                     (task "t8" 8))
                    (task "t9" 9))
                   (task "t10" 10))
                  (task "t6" 6))
                )) 4)


(test (sequest (parallel-task
                (serial-task
                 (serial-task
                  (task "t1" 1)
                  (task "t2" 2))
                 (task "t3" 3))
                (serial-task
                 (serial-task
                  (task "t4" 4)
                  (task "t5" 5))
                 (parallel-task
                  (serial-task
                   (serial-task
                    (serial-task
                     (task "t7" 7)
                     (task "t8" 8))
                    (task "t9" 9))
                   (task "t10" 10))
                  (task "t6" 6))))) 4)


(test (sequest (serial-task
                (serial-task
                 (serial-task
                  (task "t1" 1)
                  (task "t2" 2))
                 (task "t3" 3))
                (serial-task
                 (parallel-task
                  (serial-task
                   (serial-task
                    (serial-task
                     (task "t7" 7)
                     (task "t8" 8))
                    (task "t9" 9))
                   (task "t10" 10))
                  (task "t6" 6))
                 (serial-task
                  (task "t4" 4)
                  (task "t5" 5))))) 4)


(test (sequest (parallel-task (serial-task (task "t11" 11) (serial-task (task "t12" 12) (task "t13" 13))) (serial-task (task "t14" 14) (task "t15" 15)))) 3)
(test (sequest (serial-task (task "t16" 16) (parallel-task (task "t17" 17) (task "t18" 18)))) 1)
(test (sequest (serial-task (parallel-task (task "t19" 19) (task "t20" 20)) (parallel-task (task "t21" 21) (serial-task (task "t22" 22) (serial-task (task "t23" 23)(task "t24" 24)))))) 3)
(test (sequest (serial-task (parallel-task (task "t24" 24) (task "t25" 25)) (task "t26" 26))) 1)
(test (sequest (serial-task (task "t27" 27) (task "t28" 28))) 2)
(test (sequest (serial-task (task "t29" 29) (serial-task (task "t30" 30) (task "t31" 31)))) 3)
(test (sequest (parallel-task (parallel-task (task "t3" 3) (task "t4" 4)) (task "t5" 5))) 1)
(test (sequest (serial-task (serial-task (task "t1" 1) (task "t2" 2)) (parallel-task (serial-task (serial-task (task "t3" 3) (task "t4" 4)) (task "t5" 5)) (task "t6" 6)))) 3)
(test (sequest my-taskschedule) 2)

#| PARTE F |#

(test (end-time (task "t1" 1) "t1") 1)
(test (end-time (parallel-task (task "t0" 1) (task "t1" 2)) "t0") 1)
(test (end-time (parallel-task (task "t0" 1) (task "t1" 2)) "t1") 2)
(test (end-time (serial-task (task "t0" 3) (task "t1" 4)) "t0") 3)
(test (end-time (serial-task (task "t0" 3) (task "t1" 4)) "t1") 7)
(test (end-time (parallel-task (serial-task (task "t0" 5) (task "t1" 6)) (task "t2" 7)) "t0") 5)
(test (end-time (parallel-task (serial-task (task "t0" 5) (task "t1" 6)) (task "t2" 7)) "t1") 11)
(test (end-time (parallel-task (serial-task (task "t0" 5) (task "t1" 6)) (task "t2" 7)) "t2") 7)
(test (end-time (serial-task (parallel-task (serial-task (task "t0" 8) (task "t1" 9)) (task "t2" 10)) (task "t3" 11)) "t3") 28)
(test (end-time (parallel-task (parallel-task (task "t0" 5) (task "t1" 6)) (task "t2" 7)) "t2") 7)
(test (end-time my-taskschedule "t7") 9)
(test/exn (end-time my-taskschedule "collect") "tarea no encontrada")

#| PARTE G |#

(test((fold (λ (x y) x) identity identity) (task "t1" 1)) "t1")
(test((fold (λ (x y) y) length identity) (task "t1" 1)) 1)
(test((fold (λ (x y) y) (λ (x y) y) max) (parallel-task (task "t0" 1) (task "t1" 3))) 3)
(test((fold (λ (x y) y) length +) (serial-task (task "t0" 1) (task "t1" 1))) 2)

#| PARTE H |#

(test ((is-in2 "t0") (task "t0" 1)) #t)
(test ((is-in2 "t0") (task "t1" 2)) #f)
(test ((is-in2 "t2") (parallel-task (task "t2" 3) (task "t3" 2))) #t)
(test ((is-in2 "t2") (serial-task (task "t4" 4) (task "t5" 2))) #f)
(test ((is-in2 "t2") my-taskschedule) #t)
(test ((is-in2 "collect") my-taskschedule) #f)

(test (length2 (task "t0" 5)) 5)
(test (length2 (parallel-task (task "t1" 6) (task "t2" 5))) 6)
(test (length2 (parallel-task (task "t3" 6) (task "t4" 7))) 7)
(test (length2 (serial-task (task "t5" 7) (task "t6" 1))) 8)
(test (length2 (serial-task (task "t7" 1) (task "t8" 7))) 8)
(test (length2 my-taskschedule) 12)

(test (longest2 (task "t0" 3)) (cons "t0" 3))
(test (longest2 (parallel-task (task "t1" 1) (task "t2" 2))) (cons "t2" 2))
(test (longest2 (parallel-task (task "t3" 3) (task "t4" 1))) (cons "t3" 3))
(test (longest2 (serial-task (task "t5" 2) (task "t6" 1))) (cons "t5" 2))
(test (longest2 (serial-task (task "t7" 1) (task "t8" 2))) (cons "t8" 2))
(test (longest2 my-taskschedule) (cons "t5" 6))