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

; filter out dodgy characters from a string

#lang scheme
(require "list.ss")
(provide (all-defined-out))

(define white-list (string->list "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890 .,'!?-#:@"))

; start slow and stupid
(define (filter-string s)
  (foldl
   (lambda (c r)
     (if (list-contains? white-list c)
         (string-append r (string c))
         r))
   ""
   (string->list s)))

(define (unit-test)
  (when (not (string=? (filter-string "should Be ok123") "should Be ok123"))
        (error "oops"))
  (when (not (string=? (filter-string "<foo> b&a_r") "foo bar"))
        (error "oops")))


(printf "unit testing filter-string.ss~n")
(unit-test)
  
