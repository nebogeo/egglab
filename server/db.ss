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
(require "logger.ss" "utils.ss")
(require racket/date)

(define (setup db)
  (exec/ignore db "create table state ( id integer primary key autoincrement, population varchar, replicate integer, phase varchar, read_head integer, generation integer)")
  (exec/ignore db "create table egg ( id integer primary key autoincrement, population varchar, replicate integer, time_stamp varchar, player_id integer, fitness real, tests integer, generation integer, parent integer, image varchar, x_pos real, y_pos real, genotype varchar )")
  (exec/ignore db "create table player ( id integer primary key autoincrement, time_stamp varchar, name varchar, played_before integer, age_range integer )")
  (exec/ignore db "create table egghunt ( id integer primary key autoincrement, background varchar, challenger varchar, message varchar, score integer, timestamp varchar)")
  (exec/ignore db "create table egghunt_egg ( id integer primary key autoincrement, egghunt_id integer, egg_id integer, x integer, y integer)")
  (exec/ignore db "create table high_scores ( id integer primary key autoincrement, player_id int, player_name varchar, average_score real, population varchar, replicate int, generation int )")
;;  (exec/ignore db "create table egghunt_score ( id integer primary key autoincrement. egghunt_id integer, egg_id integer, est_clicked_time integer)")
  )

;; initialise the state
(define (check/init-state db population replicate)
  (let ((s (select db "select * from state where population=? and replicate=?"
                   population replicate)))
    (when (null? s)
          (msg "adding state for" population replicate)
          (insert-state db population replicate "init" 0 0))))

(define (insert-state db population replicate phase read_head generation)
  (insert db "insert into state values (NULL, ?, ?, ?, ?, ?)"
          population replicate phase read_head generation))

(define (get-state db population replicate key)
  (vector-ref
   (cadr (select db
                 (string-append "select " key " from state where population=? and replicate=?")
                 population replicate))
   0))

(define (set-state db population replicate key value)
  (exec/ignore
   db (string-append
       "update state set " key "=? where population=? and replicate=?")
   value population replicate))

(define (insert-egg db population replicate time-stamp player-id fitness
                    tests generation parent image x-pos y-pos genotype)
  (insert
   db "insert into egg values (NULL, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
   population replicate time-stamp player-id fitness
   tests generation parent image x-pos y-pos genotype))

(define (update-egg db population replicate id fitness)
  (insert
   db "update egg set fitness=fitness+?, tests=tests+1 where id = ?" fitness id))

(define (insert-player db time-stamp name played-before age-range)
  (insert
   db "insert into player values (null, ?, ?, ?, ?)"
   time-stamp name (if (equal? played-before "false") "0" "1") age-range))

(define (update-player db player-id name played-before age-range)
  (exec/ignore
   db "update player set name=?, played_before=?, age_range=? where id = ?"
   name (if (equal? played-before "false") "0" "1") age-range
   player-id))

(define (insert-score db player-id name average-score population replicate generation)
  (exec/ignore
   db "insert into high_scores values (null, ?, ?, ?, ?, ?, ?)"
   player-id name average-score population replicate generation))

(define (insert-stats db time-stamp egg-count av-fitness max-fitness min-fitness)
  (insert
   db "insert into stats values (NULL, ?, ?, ?, ?, ?)"
   time-stamp egg-count av-fitness max-fitness min-fitness))

(define (insert-egghunt db background challenger message score)
  (insert
   db "insert into egghunt values (NULL, ?, ?, ?, ?, ?)"
   background challenger message score (timestamp-now)))

(define (insert-egghunt-egg db egghunt-id egg-id x y)
  (insert
   db "insert into egghunt_egg values (NULL, ?, ?, ?, ?)"
   egghunt-id egg-id x y))

(define (ms->frac ms)
  (modulo (inexact->exact (round ms)) 1000))

(define (timestamp-now)
  (let* ((ms (current-inexact-milliseconds))
         (t (seconds->date (inexact->exact (round (/ ms 1000))))))
    (string-append
     (number->string (date-year t)) "-"
     (substring (number->string (+ (date-month t) 100)) 1 3) "-"
     (substring (number->string (+ (date-day t) 100)) 1 3) " "
     (substring (number->string (+ (date-hour t) 100)) 1 3) ":"
     (substring (number->string (+ (date-minute t) 100)) 1 3) ":"
     (substring (number->string (+ (date-second t) 100)) 1 3) "."
     ;; get fractional second from milliseconds
     (substring (number->string (+ (ms->frac ms) 1000)) 1 4)
     )))


;; random selection of count entities
(define (hiscores db population count)
  (let ((s (select
            db (string-append
                "select p.player_name, p.average_score, p.generation from high_scores as p "
                "where p.population = ? "
                "order by p.average_score limit ?")
            population count)))
    (if (null? s)
        '()
        (map
         (lambda (i)
           (list (vector-ref i 0) (vector-ref i 1) (vector-ref i 2)))
         (cdr s)))))

(define (get-egghunt db egghunt-id)
  (let ((s (select
            db (string-append
                "select h.background, h.challenger, h.message from egghunt as h "
                "where h.id = ?")
            egghunt-id)))
    (if (null? s)
        '()
        (map
         (lambda (i)
           (list (vector-ref i 0)
                 (vector-ref i 1)
                 (vector-ref i 2)))
         (cdr s)))))

(define (get-egghunt-eggs db egghunt-id)
  (let ((s (select
            db (string-append
                "select egg.genotype, e.x, e.y from egghunt_egg as e "
                "join egg as egg on egg.id = e.egg_id "
                "where e.egghunt_id = ?")
            egghunt-id)))
    (if (null? s)
        '()
        (map
         (lambda (i)
           (list (vector-ref i 0)
                 (vector-ref i 1)
                 (vector-ref i 2)))
         (cdr s)))))


(define (open-db db-name)
  (if (file-exists? (string->path db-name))
      (begin
        (display "open existing db")(newline)
        (open (string->path db-name)))
      (begin
        (display "making new db")(newline)
        (let ((db (open (string->path db-name))))
          (setup db)
          db))))

(define (unit-tests)
  ;; db
  (msg "testing db")


  )
    ;;(define (insert-stats db egg_count time_stamp av_fitness max_fitness min_fitness)

;;(unit-tests)
