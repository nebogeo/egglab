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

(define (pop-add db image game genotype fitness)
  (insert-entity db "pop" "egg" "gaia"
                 (list
                  (ktv "image" "varchar" image)
                  (ktv "game" "varchar" game)
                  (ktv "genotype" "varchar" genotype)
                  (ktv "fitness" "real" fitness)))
  ;(pop-cull db table 256)
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
(define (pop-sample db image count thresh)
  (let ((s (db-select
            db (string-append
                "select g.value from pop_entity as e "
                "join pop_value_varchar as g on (g.entity_id = e.entity_id) and (g.attribute_id = 'genotype') "
                "join pop_value_real as v on (v.entity_id = e.entity_id) and (v.value > ?) "
                "where entity_type = ? order by random() limit ?")
            (inexact->exact (round (fitness-thresh db thresh))) "egg" count)))
    (if (null? s)
        '()
        (map
         (lambda (i)
           (vector-ref i 0))
         (cdr s)))))

;(define (pop-cull db table size)
;  (let ((s (db-select
;            db (string-append
;                "select entity_id from " table
;                "_entity where entity_type = ? order by  limit ?")
;            type count)))
