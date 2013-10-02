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

;; A server request interface
;; We only want to ever call commands in the registered requests,
;; and generally need to be careful - never executing data from
;; external sources directly...

#lang scheme

(provide (all-defined-out))
(require web-server/http/response-structs "filter-string.ss" "list.ss")

(define (pluto-response txt)
  (response/full
   200                ; code
   #"Okay"            ; message
   (current-seconds)  ; seconds
   #"text/javascript" ; mime type
   '()                ; headers
   (list (string->bytes/utf-8 txt)))) ; body

;; a request is a name and a list of arguments
(define (req name args) (list name args))
(define (req-name r) (list-ref r 0))
(define (req-args r) (list-ref r 1))

;; get the argument by name from the request
(define (req-arg r n)
  (let ((kv (assq n (req-args r))))
    (cond
     (kv (cdr kv))
     (else
      (printf "unknown arg ~a on request ~a~n" n (req-name r))))))

;; check for the existance of an argument
(define (req-has-arg? r n)
  (assq n (req-args r)))

;; a register is a request and the procedure to call
(define (register req proc) (list req proc))
(define (register-req r) (list-ref r 0))
(define (register-proc r) (list-ref r 1))

; builds the argument list from the registered requests
(define (request-run reg req)
  (apply (register-proc reg)
         (map
          (lambda (arg)
            (if (req-has-arg? req arg)
                ; use the passed in value, and filter it
                ; for dangerous characters before we go
                ; any further
                (filter-string (req-arg req arg))
                '())) ; nothing found... ?
          (req-args (register-req reg)))))

;; look up this request in the registry and run it
(define (request-dispatch reg req)
  (cond
   ((null? reg) (printf "unknown command ~a~n" (req-name req))
    (string-append "unknown command "
                   (symbol->string (req-name req))))
   ((eq? (req-name (register-req (car reg))) (req-name req))
    (request-run (car reg) req))
   (else
    (request-dispatch (cdr reg) req))))
