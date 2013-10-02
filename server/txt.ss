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

;; this is bonkers - what am I missing here?

(define (scheme->txt v)
  (cond
    ((number? v) (number->string v))
    ((symbol? v) (string-append "'" (symbol->string v)))
    ((string? v) (string-append "\"" v "\""))
    ((boolean? v) (if v "#t" "#f"))
    ((list? v)
     (cond
       ((null? v) "'()")
       (else
        (list->txt v))))
    (else (printf "scheme->txt, unsupported type for ~a~n" v) 0)))

(define (list->txt l)
  (define (_ l s)
    (cond
     ((null? l) s)
     (else
      (_ (cdr l)
         (string-append
          s
          (if (not (string=? s "")) " " "")
          (scheme->txt (car l)))))))
  (string-append "(" (_ l "") ")"))
