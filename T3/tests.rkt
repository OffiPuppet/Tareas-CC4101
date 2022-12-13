#lang play
(require "T3.rkt")

(print-only-errors #t)

;; --------- [ Parte 1 ] ---------

(test (const? (parse '{ if0 {+ 1 2} {+ 3 45} {- 4 6}})) #t)
(test (const? (parse '{with {x 18} 3})) #t)
(test (const? (parse '{{fun {x} x} 19})) #f)
(test (const? (parse '{fun {x} 10})) #f)
(test (const? (parse '{with {x 5} {+ 1 x}})) #f)

(test (fold-consts (parse '{if0 {+ 1 2} {+ 3 45} {- 4 6}})) (num -2))
(test (fold-consts (parse '{with {x {+ 2 3}} {+ 1 x}})) (with 'x (num 5) (add (num 1) (id 'x))))
(test (fold-consts (parse '{{fun {x} {+ 2 3}} 7})) (app (fun 'x (add (num 2) (num 3))) (num 7)))
(test (fold-consts (parse '{with {z {+ 1 1}} z})) (with 'z (num 2) (id 'z)))
(test (fold-consts (parse '{with {x 3} {if0 {+ 3 1} {+ 2 7} {- {with {y 2} {+ 3 2}} 4}}})) (num 1))
(test (fold-consts (parse '{if0 {- 4 9} {with {x 5} {+ {+ x 3} 4}} {- 7 3}})) (if0 (num -5) (with 'x (num 5) (add (add (id 'x) (num 3)) (num 4))) (num 4)))
(test (fold-consts (parse '{{fun {y} {+ 4 4}} {+ 1 7}})) (app (fun 'y (add (num 4) (num 4))) (num 8)))
(test (fold-consts (parse '{with {y {fun {x} 16}} 2})) (with 'y (fun 'x (num 16)) (num 2)))

;; --------- [ Parte 2 ] ---------

(test (propagate-consts (parse '{with {x 3} {with {y x} {+ x y}}})) (with 'y (num 3)(add (num 3)(id 'y))))
(test (propagate-consts (parse '{{fun {x} x} 7})) (app (fun 'x (id 'x)) (num 7)))
(test (propagate-consts (parse '{if0 {+ 1 2} {+ 3 17} {- 4 6}})) (if0 (add (num 1) (num 2)) (add (num 3) (num 17)) (sub (num 4) (num 6))))
(test (propagate-consts (parse '{with {x 23} {fun {y} x}})) (fun 'y (num 23)))
(test (propagate-consts (parse '{with {x 7} {+ x x}})) (add (num 7) (num 7)))
(test (propagate-consts (parse '{with {y 2} {+ {with {y 3} y} y}})) (add (num 3) (num 2)))
(test (propagate-consts (parse '{with {x {with {x 3} x}}x}))(with 'x (num 3) (id 'x)))
(test (propagate-consts (parse '{with {y 1}{+ {with {y 6} y} y}}))(add (num 6) (num 1)))
(test (propagate-consts (parse '{with {x {with {x 1} x}} x})) (with 'x (num 1) (id 'x)))
(test (propagate-consts (with 'x (num 1) (with 'y (num 2) (with 'z (num 3) (add (id 'x) (add (id 'y) (id 'z))))))) (add (num 1) (add (num 2) (num 3))))
(test (propagate-consts (parse '{with {x {with {y 2} y}} {+ 7 x}})) (with 'x (num 2) (add (num 7) (id 'x))))
(test (propagate-consts (parse '{{fun {x} x} 8})) (app (fun 'x (id 'x)) (num 8)))

;; --------- [ Parte 3 ] ---------

(test (cf&p (parse '{if0 {+ 1 2} {+ 3 15} {- 4 6}})) (num -2))
(test (cf&p (parse '{with {x {+ 6 7}}{with {y {+ x 17}}{with {z {+ y 9}}{+ 3 z}}}})) (num 42))
(test (cf&p (parse '{{fun {x} x} 7})) (app (fun 'x (id 'x)) (num 7)))
(test (cf&p (parse '{with {y 6} {with {z 5} {with {y 5} {+ y z}}}})) (num 10))
(test (cf&p (parse '{with {x {+ 5 {{fun {y} y} 3}}} {+ x 8}})) (with 'x (add (num 5) (app (fun 'y (id 'y)) (num 3))) (add (id 'x) (num 8))))
(test (cf&p (parse '{with {y 2} {with {z 3} {with {y 4} {+ y z}}}})) (num 7))
(test (cf&p (parse '{if0 {+ 0 0} {{fun {x} x} 7} {with {x {- 1 10}} {+ x x}}})) (if0 (num 0) (app (fun 'x (id 'x)) (num 7)) (num -18)))