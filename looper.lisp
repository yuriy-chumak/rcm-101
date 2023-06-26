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
                        (else
                           (print "unknown escape code " third "(" (string third) ")") 0)))))

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
(print "adddress: " (car line))

; connect:
(send "connect")
(define line (try-parse skipper (cdr line) #false)) ; < Attempting to connect to ...
(define line (try-parse skipper (cdr line) #false)) ; < Connection successful

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
         (define numbers (substring buffer 114 125))
         (define bytes (list->bytevector (map (lambda (s) (string->number s 16)) (c/ / numbers))))
         (print (syscall 201 "%F %H:%M:%S") ": " (* (bytevector->float bytes 0) #i10000) " мкЗв/ч"))

      (loop stream (++ timestamp))))

; done:
(send "exit")
;; (system (map c-string (list "/usr/bin/kill" (number->string pid) "-9")))
(close-port (car Out))