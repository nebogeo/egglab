#lang scheme/base
;; Naked on Pluto Copyright (C) 2010 Aymeric Mansoux, Marloes de Valk, Dave Griffiths
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.
;;
;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(require "db.ss" "utils.ss")
(provide (all-defined-out))

(require (planet jaymccarthy/sqlite:5:1/sqlite))

(define pop-size 1024)
(define av-record-count 0)
(define av-record-period 1)

(define (pop-add db population replicate player-id fitness individual-fitness generation parent image genotype)
  (exec/ignore db "begin transaction")
  (let ((timestamp (timestamp-now)))
    (insert-egg
     db population replicate timestamp
     player-id fitness individual-fitness generation parent image genotype)
    (set! av-record-count (+ av-record-count 1))
    (when (>= av-record-count av-record-period)
          (set! av-record-count 0)
          (let ((stats
                 (cadr (select
                        db (string-append
                            "select count(e.fitness), avg(e.fitness), max(e.fitness), min(e.fitness) from egg as e "
                            "where e.population = ? and "
                            "e.replicate = ?")
                        population replicate))))
            (insert-stats
             db population replicate timestamp
             (vector-ref stats 0) ;; count
             (inexact->exact (round (vector-ref stats 1))) ;; average
             (inexact->exact (round (vector-ref stats 2))) ;; max
             (inexact->exact (round (vector-ref stats 3)))))) ;; min
    (exec/ignore db "end transaction")
    '("ok")))


;; return a bunch of (id genome) lists for inheritence viz

(define (family-tree db id)
  (get-family-tree db (list id #f)))

(define (get-family-tree db egg)
  (let ((p (get-parent db egg)))
    (msg p)
    (if (null? p) '()
        (cons (list egg (get-children db egg))
              (get-family-tree db (car p))))))

(define (get-parent db egg)
  (let ((s (select db "select e.parent, e.genotype from egg as e where e.id = ? " (car egg))))
    (if (null? s)
        '()
        (map
         (lambda (i)
           (list (vector-ref i 0) ""))
         (cdr s)))))

(define (get-children db egg)
  (let ((s (select db "select e.id, e.genotype from egg as e where e.parent = ? " (car egg))))
    (if (null? s)
        '()
        (map
         (lambda (i)
           (list (vector-ref i 0) ""))
         (cdr s)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; game stuff

(define (player db population replicate name score played-before age-range)
  (list
   "player-id"
   (insert-player
    db population replicate (timestamp-now) name score played-before age-range)))

(define (pop-unit-tests)
  ;; db
  (msg "testing db")
  (define db (open-db "unit-test.db"))

  (let ((id (cadr (player db "pop1" 0 "dave" 100 #t 3))))
    (for ((i (in-range 0 10)))
         (pop-add db "pop1" 0 id (random 1000) 200 1 0 "img" (string-append "(foo" (number->string i) ")")))

    (msg (sample-eggs-from-top db "pop1" 0 2 3))
    (msg (top-eggs db "pop1" 0 10))
    (msg (get-stats db "pop1" 0 10))

    ))
