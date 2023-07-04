#!/usr/bin/env ol

(if (null? *vm-args*)
   (runtime-error "Usage: looper.lisp" "XX:XX:XX:XX:XX:XX"))

; main
(define In (pipe #b01))
(define Out (pipe #b10))
(define pid (system (map c-string (c/ / (string-append "/usr/bin/gatttool -b " (car *vm-args*) " -I"))) In Out))

; custom reverse
(define (rev list tail)
   (let loop ((old list) (new tail))
      (if (null? old)
         new
         (loop (cdr old) (cons (car old) new)))))
(import (otus ffi))

; стрим, который удаляет из себя ESC последовательности
(define (ansi2txt in)
   (let loop ((out #null) (in in))
      (cond
         ((null? in) (reverse out))
         ((function? in) 
            (if (null? out)
            then
               (delay (ansi2txt (in)))
            else
               (rev out (delay (ansi2txt (in))))))
         (else
            (if (eq? (car in) 27) ; escape code
            then
               (define second (cadr in)) ; assert second == 91, #\[
               (define third (caddr in)) ; ansi escape code
               (loop out (drop in (+ 2
                     (case third
                        (#\K 1) ; clear
                        (#\A 1) ; move up
                        (#\C 1) ; move right
                        (#\0 (let do ((n 2) (in (cdddr in)))
                                 (if (eq? (car in) #\m) n (do (++ n) (cdr in))))) ; todo: process functions
                        (else 0)))))
                           ;; (print "unknown escape code " third "(" (string third) ")") 0)))))

            else
               (loop (cons (car in) out) (cdr in)))))))

(import (owl parse))
(define prompt
   (let-parse* (
         (skip (imm #\[))
         (address (greedy+ (rune-if (lambda (x) (has?
                     '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\A #\B #\C #\D #\E #\F #\:) x)))))
         (skip (imm #\]))
         (skip (word "[LE]> " #f)))
      (runes->string address)))

(define (imm13! l r p ok)
   (if (and (pair? r)
            (eq? (car r) 13))
      (ok (cons (car r) l) (cdr r) (+ p 1) (car r))
      (backtrack l r p #eof)))

(define skipper
   (let-parse* (
         (skip (lazy* rune))
         (skip (word "> " #t)))
      #true))

(define parser2
   (let-parse* (
         (command rest-of-line) ; command echo
         (messages (greedy+ (let-parse* (
               (skip prompt)
               (skip imm13!)
               (message rest-of-line))
            (runes->string message))))
         (skip prompt))
      messages))

(define (send . args)
   (apply print-to (cons (cdr In) args)))

; run
(define line (try-parse prompt (ansi2txt (port->bytestream (car Out))) #false))
(print "address: " (car line))

; connect:
(send "connect")
(define line (try-parse skipper (cdr line) #false)) ; < Attempting to connect to ...
(define line (try-parse skipper (cdr line) #false)) ; < Connection successful

; open database:
; (define *features* (cons 'sqlite-log-debug (features))) ; temp, enable debug
(import (lib sqlite))
(define database (make-sqlite3))
(sqlite3_open "radiacode-101.sqlite" database)

(define (db:value . args) (apply sqlite:value (cons database args)))
(define (db:query . args) (apply sqlite:query (cons database args)))

(db:query "CREATE TABLE IF NOT EXISTS radiation (
   id INTEGER PRIMARY KEY

,  background REAL
,  background_raw REAL
,  cps REAL
,  cps_raw REAL
,  timestamp DATETIME DEFAULT CURRENT_TIMESTAMP
)")

; start receiving data:
(send "char-write-req 0x0011 0100") (sleep 100)
(define line (try-parse parser2 (cdr line) #false))
(print line)

(let loop ((stream (cdr line)) (timestamp 0))
   ; receive data block:
   (define event (string-append
      "08000000260800"
      (number->string (+ #x80 (mod timestamp 32)) 16)
      "00010000"))
   (send "char-write-req 000e " event) (sleep 100)
   (define line (try-parse parser2 stream #false))

   ; handle data block:
   (when line
      (define answer (car line))
      (define stream (cdr line))

      ; good answer:
      (when (and (list? answer) (string-eq? (car answer) "Characteristic value was written successfully"))
         (define buffer (fold  string-append "" (map (lambda (s) (substring s 36)) (cdr answer))))

         (define numbers1 (substring buffer 114 125))
         (define bytes1 (list->bytevector (map (lambda (s) (string->number s 16)) (c/ / numbers1))))
         (define background (* (bytevector->float bytes1 0) #i1000000))

         (define numbers2 (substring buffer 159 170))
         (define bytes2 (list->bytevector (map (lambda (s) (string->number s 16)) (c/ / numbers2))))
         (define background_raw (* (bytevector->float bytes2 0) #i1000000))

         (define numbers3 (substring buffer 198 209))
         (define bytes3 (list->bytevector (map (lambda (s) (string->number s 16)) (c/ / numbers3))))
         (define cps_raw (bytevector->float bytes3 0))

         (define numbers4 (substring buffer 69 80))
         (define bytes4 (list->bytevector (map (lambda (s) (string->number s 16)) (c/ / numbers4))))
         (define cps (bytevector->float bytes4 0))

         (print (syscall 201 "%F %H:%M:%S") " : " (/ background #i100) " мкЗв/ч" " ("
               (/ background_raw #i100) ") / "
               cps " (" cps_raw ")"
         )
         (db:query "INSERT INTO radiation (background, background_raw, cps, cps_raw) VALUES (?,?,?,?)" background background_raw cps cps_raw) )

      (loop stream (++ timestamp))))

; done:
(send "exit")
(sqlite3_close database)
;; (system (map c-string (list "/usr/bin/kill" (number->string pid) "-9")))
(close-port (car Out))
