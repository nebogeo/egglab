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

; convert scheme values into equivilent json strings

(define (scheme->json v)
  (cond
   ((number? v) (number->string v))
   ((symbol? v) (string-append "\"" (symbol->string v) "\""))
   ((string? v) (string-append "\"" v "\""))
   ((boolean? v) (if v "true" "false"))
   ((list? v)
    (cond
     ((null? v) "[]")
     (else
      ; if it quacks like an assoc list...
      (if (and (not (null? v)) (not (list? (car v))) (pair? (car v)))
          (assoc->json v)
          (list->json v)))))
   (else (printf "value->js, unsupported type for ~a~n" v) 0)))

(define (list->json l)
  (define (_ l s)
    (cond
     ((null? l) s)
     (else
      (_ (cdr l)
         (string-append s
                        (if (not (string=? s "")) ", " "")
                        (scheme->json (car l)))))))
  (string-append "[" (_ l "") "]"))

; ((one . 1) (two . "three")) -> { "one": 1, "two": "three }

(define (assoc->json l)
  (define (_ l s)
    (cond
     ((null? l) s)
     (else
      (let ((token (scheme->json (car (car l))))
            (value (scheme->json (cdr (car l)))))
        (_ (cdr l) (string-append s (if (not (string=? s "")) "," "")
                                  "\n" token ": " value))))))
  (string-append "{" (_ l "") "\n" "}"))
