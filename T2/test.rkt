#lang play
(require "T2.rkt")

(print-only-errors #t)

;;====== parse-Cond TESTS ======

;; > constructor
(define my-s-Cond> '(> terremotos empanadas))
(define my-Cond> (> 'terremotos 'empanadas))

;; < constructor
(define my-s-Cond< '(< terremotos empanadas))
(define my-Cond< (< 'terremotos 'empanadas))

;; = constructor
(define my-s-Cond= '(= terremotos empanadas))
(define my-Cond= (= 'terremotos 'empanadas))

;; & constructor

(define my-s-Cond& '(& (= terremotos empanadas) (< empanadas choripan)))
(define my-Cond& (& (= 'terremotos 'empanadas) (< 'empanadas 'choripan)))

;; orb constructor
(define my-s-Cond-orb '(orb (= terremotos empanadas) (< empanadas choripan)))
(define my-Cond-orb (orb (= 'terremotos 'empanadas) (< 'empanadas 'choripan)))


;;casos enunciado
(define my-s-Cmd '(CREATE toki-toki-ti (mesa terremotos empanadas)
        (INSERT (3 5 4) toki-toki-ti
                (INSERT (2 2 4) toki-toki-ti
                        (INSERT (1 2 3) toki-toki-ti
                                (FROM toki-toki-ti SELECT regs WHERE (= empanadas 4)))))))



(define my-Cmd (CREATE 'toki-toki-ti '(mesa terremotos empanadas)
        (INSERT '(3 5 4) 'toki-toki-ti
                (INSERT '(2 2 4) 'toki-toki-ti
                        (INSERT '(1 2 3) 'toki-toki-ti
                                (FROM 'toki-toki-ti 'SELECT 'regs 'WHERE (= 'empanadas 4)))))))


;;caso similar al del enunciado
(define my-s-Cmd2 '(CREATE A (a b c)
                           (INSERT (1 2 3) A
                                   (INSERT (4 5 6) A
                                           (FROM A SELECT regs WHERE (= b 5))))))



(define my-Cmd2 (CREATE 'A '(a b c)
                        (INSERT '(1 2 3) 'A
                                (INSERT '(4 5 6) 'A
                                        (FROM 'A 'SELECT 'regs 'WHERE (= 'b 5))))))


;; caso inserción doble con mismo registro en misma tabla
(define my-s-Cmd3 '(CREATE toki-toki-ti (mesa terremotos empanadas)
        (INSERT (3 5 4) toki-toki-ti
                (INSERT (3 5 4) toki-toki-ti
                        (INSERT (1 2 3) toki-toki-ti
                                (FROM toki-toki-ti SELECT regs WHERE (= empanadas 4)))))))


(define my-Cmd3 (CREATE 'toki-toki-ti '(mesa terremotos empanadas)
        (INSERT '(3 5 4) 'toki-toki-ti
                (INSERT '(3 5 4) 'toki-toki-ti
                        (INSERT '(1 2 3) 'toki-toki-ti
                                (FROM 'toki-toki-ti 'SELECT 'regs 'WHERE (= 'empanadas 4)))))))



;; caso de borde: más de un CREATE
(define my-s-Cmd-genshin-pokemon '(CREATE GENSHIN (tabibito paimon itto)
                                        (INSERT (1 2 3) GENSHIN
                                                (INSERT (4 5 6) GENSHIN
                                                        (CREATE POKEMON (zeraora tyranitar pachirisu)
                                                                (INSERT (7 8 9) POKEMON
                                                                        (FROM GENSHIN SELECT regs WHERE (= tabibito 1))))))))



(define my-Cmd-genshin-pokemon (CREATE 'GENSHIN '(tabibito paimon itto)
                                     (INSERT '(1 2 3) 'GENSHIN
                                             (INSERT '(4 5 6) 'GENSHIN
                                                     (CREATE 'POKEMON '(zeraora tyranitar pachirisu)
                                                             (INSERT '(7 8 9) 'POKEMON
                                                                     (FROM 'GENSHIN 'SELECT 'regs 'WHERE (= 'tabibito 1))))))))



(define my-s-Cmd-genshin-pokemon2 '(CREATE GENSHIN (tabibito paimon itto)
                                        (INSERT (1 2 3) GENSHIN
                                                (INSERT (4 5 6) GENSHIN
                                                        (CREATE POKEMON (zeraora tyranitar pachirisu)
                                                                (INSERT (7 8 9) POKEMON
                                                                        (FROM POKEMON SELECT regs WHERE (= zeraora 7))))))))



(define my-Cmd-genshin-pokemon2 (CREATE 'GENSHIN '(tabibito paimon itto)
                                     (INSERT '(1 2 3) 'GENSHIN
                                             (INSERT '(4 5 6) 'GENSHIN
                                                     (CREATE 'POKEMON '(zeraora tyranitar pachirisu)
                                                             (INSERT '(7 8 9) 'POKEMON
                                                                     (FROM 'POKEMON 'SELECT 'regs 'WHERE (= 'zeraora 7))))))))



;; se consulta una tabla inexistente
(define my-s-Cmd-failed-query '(CREATE POKEMON (kyogre rayquaza groudon)
                                       (INSERT (1 2 3) POKEMON
                                               (INSERT (4 5 6) POKEMON
                                                       (FROM POKEMON1 SELECT regs WHERE (= rayquaza 2))))))


;; se consulta una tabla inexistente con mas de un create
(define my-s-Cmd-failed-query2 '(CREATE POKEMON (kyogre rayquaza groudon)
                                        (INSERT (1 2 3) POKEMON
                                                (INSERT (4 5 6) POKEMON
                                                        (CREATE POKEMON1 (jirachi azelf mesprit)
                                                                (INSERT (7 8 9) POKEMON1
                                                                        (FROM POKEMON2 SELECT regs WHERE (= azelf 8))))))))



;; se inserta en una tabla inexistente
(define my-s-Cmd-failed-insertion '(CREATE POKEMON (entei raikou suicune)
                                           (INSERT (1 2 3) POKEMON1
                                                   (INSERT (4 5 6) POKEMON
                                                           (FROM POKEMON SELECT regs WHERE (= entei 1))))))


(define my-Cmd-failed-insertion (CREATE 'POKEMON '(entei raikou suicune)
                                           (INSERT '(1 2 3) 'POKEMON1
                                                   (INSERT '(4 5 6) 'POKEMON
                                                           (FROM 'POKEMON 'SELECT 'regs 'WHERE (= 'entei 1))))))


;; se inserta en una tabla inexistente con más de un CREATE
(define my-s-Cmd-failed-insertion2 '(CREATE POKEMON (bulbasaur charmander squirtle)
                                        (INSERT (1 2 3) POKEMON
                                                (INSERT (4 5 6) POKEMON
                                                        (CREATE POKEMON1 (articuno zapdos moltres)
                                                                (INSERT (7 8 9) POKEMON2
                                                                        (FROM POKEMON1 SELECT regs WHERE (= articuno 7))))))))

(define my-Cmd-failed-insertion2 (CREATE 'POKEMON '(bulbasaur charmander squirtle)
                                        (INSERT '(1 2 3) 'POKEMON
                                                (INSERT '(4 5 6) 'POKEMON
                                                        (CREATE 'POKEMON1 '(articuno zapdos moltres)
                                                                (INSERT '(7 8 9) 'POKEMON2
                                                                        (FROM 'POKEMON1 'SELECT 'regs 'WHERE (= 'articuno 7))))))))



;; se inserta un registro con una aridad inconsistente
(define my-s-Cmd-failed-arity '(CREATE GENSHIN (shogun gorou zhongli)
                                       (INSERT (1 2 3 4) GENSHIN
                                               (INSERT (5 6 7) GENSHIN
                                                       (FROM GENSHIN SELECT regs WHERE (= shogun 1))))))

;; se inserta un registro con una aridad inconsistente con más de un CREATE
(define my-s-Cmd-failed-arity2 '(CREATE GENSHIN (nahida nilou cyno)
                                        (INSERT (1 2 3) GENSHIN
                                                (INSERT (4 5 6) GENSHIN
                                                        (CREATE GENSHIN1 (bennett mika sayu klee)
                                                                (INSERT (7 8 9 10 11) GENSHIN1
                                                                        (FROM GENSHIN1 SELECT regs WHERE (= bennett 7))))))))

;; se consulta una columna inexistente
(define my-s-Cmd-failed-column '(CREATE GENSHIN (tartaglia capitano pantalone)
                                        (INSERT (1 2 3) GENSHIN
                                                (INSERT (4 5 6) GENSHIN
                                                        (FROM GENSHIN SELECT regs WHERE (= pierro 1))))))

;; se consulta una columna inexistente con más de un CREATE
(define my-s-Cmd-failed-column2 '(CREATE GENSHIN (pulcinella columbina sandrone)
                                        (INSERT (1 2 3) GENSHIN
                                                (INSERT (4 5 6) GENSHIN
                                                        (CREATE GENSHIN1 (arlecchino signora scaramouche)
                                                                (INSERT (7 8 9) GENSHIN1
                                                                        (FROM GENSHIN1 SELECT regs WHERE (= dottore 7))))))))




#| ==============================
            EJERCICIO 1
   ============================== |#


#| PARTE A |#

(test (parse-Cond my-s-Cond>) my-Cond>)
(test (parse-Cond '(> (1 2 3) chicha)) (> '(1 2 3) 'chicha))

(test (parse-Cond my-s-Cond<) my-Cond<)
(test (parse-Cond '(< (1 2 3) chicha)) (< '(1 2 3) 'chicha))

(test (parse-Cond my-s-Cond=) my-Cond=)
(test (parse-Cond '(= (1 2 3) chicha)) (= '(1 2 3) 'chicha))

(test (parse-Cond my-s-Cond&) my-Cond&)
(test (parse-Cond '(& (= (1 2 3) empanadas) (> empanadas choripan))) (& (= '(1 2 3) 'empanadas) (> 'empanadas 'choripan)))

(test (parse-Cond my-s-Cond-orb) my-Cond-orb)
(test (parse-Cond '(orb (= (1 2 3) empanadas) (> empanadas choripan))) (orb (= '(1 2 3) 'empanadas) (> 'empanadas 'choripan)))

(test (parse my-s-Cmd) my-Cmd)
(test (parse my-s-Cmd2) my-Cmd2)
(test (parse my-s-Cmd3) my-Cmd3)
(test (parse my-s-Cmd-genshin-pokemon) my-Cmd-genshin-pokemon)
(test (parse my-s-Cmd-failed-insertion2) my-Cmd-failed-insertion2)



#| ==============================
            EJERCICIO 2
   ============================== |#

#| PARTE A |#

(test (name-in-list '() 'kevin) #f)
(test (name-in-list '(genshin pokemon) 'kevin) #f)
(test (name-in-list '(xd kevin) 'kevin) #t)
(test (name-in-list '(kevin xd) 'kevin) #t)
(test (name-in-list '(kush (1 2 kevin)) 'kevin) #f)
(test (name-in-list '(kevin (1 2)) '(1 2)) #f)

(test (check-table (parse my-s-Cmd)) #t)
(test (check-table (parse my-s-Cmd2)) #t)
(test (check-table (parse my-s-Cmd3)) #t)
(test (check-table (parse my-s-Cmd-genshin-pokemon)) #t)
(test (check-table (parse my-s-Cmd-genshin-pokemon2)) #t)
(test/exn (check-table (parse my-s-Cmd-failed-query)) "Error Estatico A2: La tabla consultada POKEMON1 no se encuentra definida.")
(test/exn (check-table (parse my-s-Cmd-failed-query2)) "Error Estatico A2: La tabla consultada POKEMON2 no se encuentra definida.")
(test/exn (check-table (parse my-s-Cmd-failed-insertion)) "Error Estatico A1: Registro (1 2 3) insertado en tabla indefinida POKEMON1.")
(test/exn (check-table (parse my-s-Cmd-failed-insertion2)) "Error Estatico A1: Registro (7 8 9) insertado en tabla indefinida POKEMON2.")



#| PARTE B |#

(test (check-arity (parse my-s-Cmd)) #t)
(test (check-arity (parse my-s-Cmd2)) #t)
(test (check-arity (parse my-s-Cmd3)) #t)
(test (check-arity (parse my-s-Cmd-genshin-pokemon)) #t)
(test (check-arity (parse my-s-Cmd-genshin-pokemon2)) #t)
(test/exn (check-arity (parse my-s-Cmd-failed-arity)) "Error Estatico B: Registro (1 2 3 4) con aridad 4 insertado en tabla GENSHIN de aridad 3.")
(test/exn (check-arity (parse my-s-Cmd-failed-arity2)) "Error Estatico B: Registro (7 8 9 10 11) con aridad 5 insertado en tabla GENSHIN1 de aridad 4.")



#| PARTE C |#

(test (check-column (parse my-s-Cmd)) #t)
(test (check-column (parse my-s-Cmd2)) #t)
(test (check-column (parse my-s-Cmd3)) #t)
(test (check-column (parse my-s-Cmd-genshin-pokemon)) #t)
(test (check-column (parse my-s-Cmd-genshin-pokemon2)) #t)
(test/exn (check-column (parse my-s-Cmd-failed-column)) "Error Estático C: La columna pierro no está definida en la tabla GENSHIN.")
(test/exn (check-column (parse my-s-Cmd-failed-column2)) "Error Estático C: La columna dottore no está definida en la tabla GENSHIN1.")



#| PARTE D |#

(test (static-check (parse my-s-Cmd)) #t)
(test (static-check (parse my-s-Cmd2)) #t)
(test (static-check (parse my-s-Cmd3)) #t)
(test (static-check (parse my-s-Cmd-genshin-pokemon)) #t)
(test (static-check (parse my-s-Cmd-genshin-pokemon2)) #t)
(test/exn (static-check (parse my-s-Cmd-failed-insertion)) "Error Estatico A1: Registro (1 2 3) insertado en tabla indefinida POKEMON1.")
(test/exn (static-check (parse my-s-Cmd-failed-insertion2)) "Error Estatico A1: Registro (7 8 9) insertado en tabla indefinida POKEMON2.")
(test/exn (static-check (parse my-s-Cmd-failed-query)) "Error Estatico A2: La tabla consultada POKEMON1 no se encuentra definida.")
(test/exn (static-check (parse my-s-Cmd-failed-query2)) "Error Estatico A2: La tabla consultada POKEMON2 no se encuentra definida.")
(test/exn (static-check (parse my-s-Cmd-failed-arity)) "Error Estatico B: Registro (1 2 3 4) con aridad 4 insertado en tabla GENSHIN de aridad 3.")
(test/exn (static-check (parse my-s-Cmd-failed-arity2)) "Error Estatico B: Registro (7 8 9 10 11) con aridad 5 insertado en tabla GENSHIN1 de aridad 4.")
(test/exn (static-check (parse my-s-Cmd-failed-column)) "Error Estático C: La columna pierro no está definida en la tabla GENSHIN")
(test/exn (static-check (parse my-s-Cmd-failed-column2)) "Error Estático C: La columna dottore no está definida en la tabla GENSHIN1")



#| ==============================
            EJERCICIO 3
   ============================== |#

#| PARTE A |#

(test (interp-cmd (parse my-s-Cmd) empty-env) '((3 5 4) (2 2 4)))
(test (interp-cmd (parse my-s-Cmd2) empty-env) '((4 5 6)))
(test (interp-cmd (parse my-s-Cmd3) empty-env) '((3 5 4) (3 5 4)))

#| PARTE B |#

(test (run (parse my-s-Cmd)) '((3 5 4) (2 2 4)))
(test (run (parse my-s-Cmd2)) '((4 5 6)))
(test/exn (run (parse my-s-Cmd-failed-insertion)) "Error Estatico A1: Registro (1 2 3) insertado en tabla indefinida POKEMON1.")
(test/exn (run (parse my-s-Cmd-failed-insertion2)) "Error Estatico A1: Registro (7 8 9) insertado en tabla indefinida POKEMON2.")
(test/exn (run (parse my-s-Cmd-failed-query)) "Error Estatico A2: La tabla consultada POKEMON1 no se encuentra definida.")
(test/exn (run (parse my-s-Cmd-failed-query2)) "Error Estatico A2: La tabla consultada POKEMON2 no se encuentra definida.")
(test/exn (run (parse my-s-Cmd-failed-arity)) "Error Estatico B: Registro (1 2 3 4) con aridad 4 insertado en tabla GENSHIN de aridad 3.")
(test/exn (run (parse my-s-Cmd-failed-arity2)) "Error Estatico B: Registro (7 8 9 10 11) con aridad 5 insertado en tabla GENSHIN1 de aridad 4.")
(test/exn (run (parse my-s-Cmd-failed-column)) "Error Estático C: La columna pierro no está definida en la tabla GENSHIN.")
(test/exn (run (parse my-s-Cmd-failed-column2)) "Error Estático C: La columna dottore no está definida en la tabla GENSHIN1.")