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

(require "eavdb.ss")
(provide (all-defined-out))

(define (pop-add db image genotype fitness)
  (insert-entity db "pop" "egg" "gaia"
                 (list
                  (ktv "image" "varchar" image)
                  (ktv "genotype" "varchar" genotype)
                  (ktv "fitness" "real" fitness)))
  ;(pop-cull db table 256)
  )

;; random selection of count entities
(define (pop-sample db image count)
  (let ((s (db-select
            db (string-append
                "select entity_id from " "pop"
                "_entity where entity_type = ? order by random() limit ?")
            "egg" count)))
    (if (null? s)
        '()
        (map
         (lambda (i)
           (get-entity db "pop" (vector-ref i 0)))
         (cdr s)))))

;(define (pop-cull db table size)
;  (let ((s (db-select
;            db (string-append
;                "select entity_id from " table
;                "_entity where entity_type = ? order by  limit ?")
;            type count)))
