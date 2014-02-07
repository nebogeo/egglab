#!/usr//bin/env mzscheme
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

(require scheme/system
         scheme/foreign
         scheme/cmdline
         mzlib/string
         web-server/servlet
         web-server/servlet-env
         web-server/http/response-structs
         "server/filter-string.ss"
         "server/request.ss"
         "server/logger.ss"
         "server/json.ss"
         "server/utils.ss"
         "server/db.ss"
         "server/txt.ss"
         "server/pop.ss")

; a utility to change the process owner,
; assuming mzscheme is called by root.
;;(unsafe!)
;;(define setuid (get-ffi-obj 'setuid #f (_fun _int -> _int)))

(define db-name "egglab.db")
(define db (open-db db-name))
(open-log "log.txt")

(define registered-requests
  (list

   (register
    (req 'ping '())
    (lambda ()
      (pluto-response (scheme->json '("hello")))))

   (register
    (req 'add '(population replicate player-id fitness individual-fitness generation parent image genotype))
    (lambda (population replicate player-id fitness individual-fitness generation parent image genotype)
      (pluto-response
       (scheme->json
        (pop-add
         db
         population
         (string->number replicate)
         (string->number player-id)
         (string->number fitness)
         (string->number individual-fitness)
         (string->number generation)
         (string->number parent)
         ;; store in escaped JSON format so we don't ever need to eval them
         image (escape-quotes genotype))))))

   (register
    (req 'sample '(population replicate count top))
    (lambda (population replicate count top)
      (let ((samples (sample-eggs-from-top
                      db
                      population
                      (string->number replicate)
                      (string->number count)
                      (string->number top))))
        (pluto-response
         (scheme->json
          samples)))))


;   (pluto-response
;-       (string-append
;-        (scheme->txt
;-         (string-append
;-          "(list "
;-          (apply
;-           string-append
;-           (sample-eggs-from-top
;-            db
;-            population
;-            (string->number replicate)
;-            (string->number count)
;-            (string->number top)))
;-          ")"))))))


   (register
    (req 'top-eggs '(population replicate count))
    (lambda (population replicate count)
      (pluto-response
       (scheme->json
        (top-eggs db population
                  (string->number replicate)
                  (string->number count))))))

   (register
    (req 'get-stats '(population replicate count))
    (lambda (population replicate count)
      (pluto-response
       (string-append
        (scheme->json
         (get-stats
          db population (string->number replicate)
          (string->number count)))))))

   (register
    (req 'player '(population replicate name score played-before age-range))
    (lambda (population replicate name score played-before age-range)
      (pluto-response
       (scheme->json
        (player db population
                (string->number replicate)
                name
                (string->number score)
                played-before
                (string->number age-range))))))

   (register
    (req 'hiscores '(population replicate count))
    (lambda (population replicate count)
      (pluto-response

       (scheme->json
        (hiscores db population (string->number replicate) (string->number count)))

       ;(string-append
       ; (scheme->txt
       ;  (string-append
       ;   "(list "
       ;   (apply
       ;    string-append
       ;    (map
       ;     (lambda (i)
       ;       (string-append "(list '" (car i) "' " (number->string (cadr i)) ")"))
       ;     (hiscores db population (string->number replicate) (string->number count))))
       ;   ")")))

       )))

   ))

(define (start request)
  (let ((values (url-query (request-uri request))))
    (if (not (null? values))   ; do we have some parameters?
        (let ((name (assq 'fn values)))
          (if name           ; is this a well formed request?
              (request-dispatch
               registered-requests
               (req (string->symbol (cdr name))
                    (filter
                     (lambda (v)
                       (not (eq? (car v) 'fn)))
                     values)))
              (pluto-response (dbg "bad formed request thing"))))
        (pluto-response (dbg "malformed thingy")))))

(printf "server is running...~n")

; Here we become the user 'nobody'.
; This is a security rule that *only works* if nobody owns no other processes
; than mzscheme. Otherwise better create another dedicated unprivileged user.
; Note: 'nobody' must own the state directory and its files.

;(setuid 65534)

;;

(serve/servlet
 start
 ;; port number is read from command line as argument
 ;; ie: ./server.scm 8080
; #:listen-ip "192.168.2.1"
 #:listen-ip "127.0.0.1"
 #:port (string->number (command-line #:args (port) port))
 #:command-line? #t
 #:servlet-path "/egglab"
 #:server-root-path
 (build-path "client"))
