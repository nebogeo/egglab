;; Copyright (C) 2013 Dave Griffiths
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

#lang racket
(require (planet jaymccarthy/sqlite:5:1/sqlite))
(provide (all-defined-out))
(require "logger.ss")
(require racket/date)

(define (setup db)
  (exec/ignore db "create table egg ( id integer primary key autoincrement, player_id integer, fitness real, individual_fitness real, generation integer, parent integer, time_stamp varchar, population varchar, replicate integer, code varchar )")
  (exec/ignore db "create table stats ( id integer primary key autoincrement, egg_count integer, time_stamp varchar, av_fitness real, max_fitness real, min_fitness real")
  (exec/ignore db "create table player ( id integer primary key autoincrement, name varchar, population varchar, average_score real, played_before integer, age_range integer, replicate integer )")
  )

(define (insert-player db name average_score played_before age_range replicate)
  (insert
   db "insert into player values (null, ?, ?, ?, ?, ?)"
   name average_score (if (equal? played_before "false") "0" "1") age_range replicate))

(define (insert-egg db player_id fitness individual_fitness generation parent time_stamp population replicate code)
  (insert
   db "insert into egg values (NULL, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
   player_id fitness individual_fitness generation parent time_stamp population replicate code))

(define (insert-stats db egg_count time_stamp av_fitness max_fitness min_fitness)
  (insert
   db "insert into stats values (NULL, ?, ?, ?, ?, ?)"
   egg_count time_stamp av_fitness max_fitness min_fitness))

(define (timestamp-now)
  (let* ((ms (current-inexact-milliseconds))
         (t (seconds->date (/ ms 1000))))
    (string-append
     (number->string (date-year t)) "-"
     (number->string (date-month t)) "-"
     (number->string (date-day t)) " "
     (number->string (date-hour t)) ":"
     (number->string (date-minute t)) ":"
     (number->string (date-second t)) "."
     ;; get fractional second from milliseconds
     (number->string (/ (modulo (inexact->exact (* ms 1000)) 1000000) 1))
     )))

;; returns the fitness count eggs down in the ordered population
(define (get-fitness-thresh db population replicate count)
  (let ((s (db-select
            db (string-append
                "select e.fitness from egg as e where"
                "e.population = ? and "
                "e.replicate = ? and "
                "order by e.fitness ")
            population replicate thresh-fitness count)))
    (if (null? s)
        '()
        (map
         (lambda (i)
           (vector-ref i 0))
         (cdr s)))))


;; return count number of eggs from eggs with fitness higher than
;; thresh-fitness in the population and replicate specified
(define (sample-egg db  population replicate count thresh-fitness)
  (let ((s (db-select
            db (string-append
                "select e.code from egg as e where"
                "e.population = ? and "
                "e.replicate = ? and "
                "e.fitness > ? and "
                "order by random() limit ?")
            population replicate thresh-fitness count)))
    (if (null? s)
        '()
        (map
         (lambda (i)
           (vector-ref i 0))
         (cdr s)))))
