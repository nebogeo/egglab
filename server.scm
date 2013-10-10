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
         web-server/servlet
         web-server/servlet-env
         web-server/http/response-structs
         "server/request.ss"
         "server/logger.ss"
         "server/json.ss"
         "server/utils.ss"
         "server/eavdb.ss"
         "server/txt.ss"
         "server/pop.ss")

; a utility to change the process owner,
; assuming mzscheme is called by root.
;;(unsafe!)
;;(define setuid (get-ffi-obj 'setuid #f (_fun _int -> _int)))

(define db-name "egglab.db")
(define db (db-open db-name (list "pop")))
(open-log "log.txt")

(define registered-requests
  (list

   (register
    (req 'ping '())
    (lambda ()
      (pluto-response (scheme->txt '("hello")))))

   (register
    (req 'sample '(land count thresh))
    (lambda (land count thresh)
      (pluto-response
       (string-append
        (scheme->txt
         (string-append
          "(list "
          (apply
           string-append
           (pop-sample db land (string->number count) (string->number thresh)))
          ")"))))))

   (register
    (req 'stats '(land count))
    (lambda (land count)
      (pluto-response
       (string-append
        (scheme->txt
         (string-append
          "(list "
          (apply
           string-append
           (pop-stats db land (string->number count)))
          ")"))))))


   (register
    (req 'add '(image land game genotype fitness))
    (lambda (image land game genotype fitness)
      (pluto-response
       (scheme->txt
        (pop-add db image land game genotype (string->number fitness))))))
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
