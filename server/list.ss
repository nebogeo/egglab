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

#lang scheme
(provide (all-defined-out))

;; returns the list with the value removed
(define (list-remove l v)
  (define (_ l v o)
    (cond
     ((null? l) o)
     ((eq? (car l) v)
      (_ (cdr l) v o))
     (else
      (_ (cdr l) v (cons (car l) o)))))
  (_ l v '()))

(define (list-remove-equal l v)
  (define (_ l v o)
    (cond
     ((null? l) o)
     ((equal? (car l) v)
      (_ (cdr l) v o))
     (else
      (_ (cdr l) v (cons (car l) o)))))
  (_ l v '()))

;; does the list contain this value?
(define (list-contains? l v)
  (cond
   ((null? l) #f)
   ((eq? (car l) v) #t)
   (else
    (list-contains? (cdr l) v))))

;; does the list contain this value?
(define (list-contains-equal? l v)
  (cond
   ((null? l) #f)
   ((equal? (car l) v) #t)
   (else
    (list-contains-equal? (cdr l) v))))

;; limit the size of a list and optionally call
;; a procedure if we are over the max size
(define (safe-cons v l max (proc (lambda () 0)))
  (cond 
   ((< (length l) max) (cons v l))
   (else (proc) l)))

;; (list-replace '(1 2 3 4) 2 100) => '(1 2 100 4)
(define (list-replace l i v)
  (cond
    ((null? l) l)
    ((zero? i) (cons v (list-replace (cdr l) (- i 1) v)))
    (else (cons (car l) (list-replace (cdr l) (- i 1) v)))))

(define (choose l) (list-ref l (random (length l))))

(define (set-cons v l)
  (if (list-contains? l v)
      l
      (cons v l)))

(define (set-cons-equal v l)
  (if (list-contains-equal? l v)
      l
      (cons v l)))

(define (clip l c)
  (cond
   ((null? l) l)
   ((zero? c) '())
   (else (cons (car l) (clip (cdr l) (- c 1))))))
  
; (1 2 3 4) -> (0 1 2 3)
(define (limit-cons v l max)
  (clip (cons v l) max))

(define (set-cons-equal-limit v l max)
  (if (list-contains-equal? l v)
      l
      (limit-cons v l max)))
