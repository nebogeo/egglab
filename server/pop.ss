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

(require "eavdb.ss" "utils.ss")
(provide (all-defined-out))

(define pop-size 255)

(define (pop-add db image land game genotype fitness)
  (insert-entity db "pop" "egg" "gaia"
                 (list
                  (ktv "time" "real" (current-inexact-milliseconds))
                  (ktv "image" "varchar" image)
                  (ktv "land" "varchar" land)
                  (ktv "game" "varchar" game)
                  (ktv "genotype" "varchar" genotype)
                  (ktv "fitness" "real" fitness)))
  (if (< (random 100) 2)
      (pop-oldest-cull db land pop-size)
      (pop-cull db land pop-size))

  (msg (select-first
        db (string-append
            "select avg(value) from pop_value_real where attribute_id = 'fitness' ")))

  '("ok"))

(define (fitness-thresh db perc)
  (let* ((s (cadr
             (db-select
              db "select min(value), max(value) from pop_value_real where attribute_id = 'fitness'")))
         (min (vector-ref s 0))
         (max (vector-ref s 1))
         (t (/ perc 100)))
    (if (not min)
        0
        (+ min (* t (- max min))))))

;; random selection of count entities
(define (pop-sample db land count thresh)
  (let ((s (db-select
            db (string-append
                "select g.value from pop_entity as e "
                "join pop_value_varchar as g on (g.entity_id = e.entity_id) and (g.attribute_id = 'genotype') "
                "join pop_value_varchar as l on (l.entity_id = e.entity_id) and (l.attribute_id = 'land') "
                "join pop_value_real as v on (v.entity_id = e.entity_id) and (v.attribute_id = 'fitness') "
                "where entity_type = ? and l.value = ? order by random() limit ?")
            "egg" land count)))
    (if (null? s)
        '()
        (map
         (lambda (i)
           (vector-ref i 0))
         (cdr s)))))

;; top n eggs
(define (pop-stats db land count)
  (let ((s (db-select
            db (string-append
                "select g.value, v.value from pop_entity as e "
                "join pop_value_varchar as g on (g.entity_id = e.entity_id) and (g.attribute_id = 'genotype') "
                "join pop_value_varchar as l on (l.entity_id = e.entity_id) and (l.attribute_id = 'land') "
                "join pop_value_real as v on (v.entity_id = e.entity_id) and (v.attribute_id = 'fitness') "
                "where entity_type = ? and l.value = ? order by v.value desc limit ?")
            "egg" land count)))
    (if (null? s)
        '()
        (map
         (lambda (i)
           (list (vector-ref i 0) (vector-ref i 1)))
         (cdr s)))))

(define (cull db l size)
  (cond
   ((< (length l) size) l)
   (else
    (msg "deleting " (car l))
    (delete-entity db "pop" (car (car l)))
    (cull db (cdr l) size))))

;; remove lowest not in this size population
(define (pop-cull db land size)
  (let ((s (db-select
            db (string-append
                "select e.entity_id, v.value from pop_entity as e "
                "join pop_value_varchar as l on (l.entity_id = e.entity_id) and (l.attribute_id = 'land') "
                "join pop_value_real as v on (v.entity_id = e.entity_id) and (v.attribute_id = 'fitness') "
                "where entity_type = ? and l.value = ? and v.value is not null order by v.value")
            "egg" land)))
    (when (not (null? s))
          (cull
           db
           (map
            (lambda (id)
              (list (vector-ref id 0)
                    (vector-ref id 1)))
            (cdr s))
           size))))

;; remove lowest not in this size population
(define (pop-oldest-cull db land size)
  (let ((s (db-select
            db (string-append
                "select e.entity_id, v.value from pop_entity as e "
                "join pop_value_varchar as l on (l.entity_id = e.entity_id) and (l.attribute_id = 'land') "
                "join pop_value_real as v on (v.entity_id = e.entity_id) and (v.attribute_id = 'time') "
                "where entity_type = ? and l.value = ? and v.value is not null order by v.value")
            "egg" land)))
    (when (not (null? s))
          (cull
           db
           (map
            (lambda (id)
              (list
               (vector-ref id 0)
               (string-append "time: " (number->string (vector-ref id 1)))))
            (cdr s))
           size))))
