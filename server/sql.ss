#lang racket
(require (planet jaymccarthy/sqlite:5:1/sqlite))

(define test-db "test.db")

(define (clean)
  (delete-file* test-db))

(define (dostuff test)
  (define db #f)
  (with-handlers ([exn:fail? void])
    (lambda ()
      (clean)
      (set! db (open (string->path test-db)))
      (exec/ignore db "CREATE TABLE user ( user_id INTEGER PRIMARY KEY, name TEXT )")
      (insert db "INSERT INTO user (name) VALUES ('noel')")
      (insert db "INSERT INTO user (name) VALUES ('matt')")
      (insert db "INSERT INTO user (name) VALUES ('dave')")
      (close db))))


(dostuff test-db)
