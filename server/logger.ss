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

; record activities to a file for later examination

#lang scheme
(require scheme/date)

(provide (all-defined-out))

(define log-filename "")

(define (open-log filename)
  (printf "opened log: ~a~n" filename)
  (set! log-filename filename)
  (log "Started server at: " 
      (date->string (seconds->date (current-seconds)))))

(define (log . args)
  (when (not (equal? log-filename ""))
        (_log (foldl
               (lambda (txt r)
                 (cond 
                  ((string? txt) (string-append r txt))
                  ((number? txt) (string-append r (number->string txt)))))
               ""
               args))))

(define (_log txt)
  (printf "~a~n" txt)
  (let ((f (open-output-file log-filename #:exists 'append)))    
    (display (string-append (date->string 
                            (seconds->date (current-seconds)) #t)  
                            " " txt) f) 
    (newline f)
    (close-output-port f)))
