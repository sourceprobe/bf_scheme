#!/usr/bin/csi -s
; Install required modules:
; $ chicken-install srfi-69
(import (chicken io))
(import (chicken format))
(import (chicken process-context))
(import (srfi 69))
(import (srfi 4))

(define TAPE_LEN 30000)

; return hash tables to find matching braces
(define (build_jumps program)
  ; first element of stack is top of stack
  (define (build_jumps_inner i stack forward backward)
    (if (>= i (string-length program))
      (if (eq? '() stack)
	(cons forward backward)
	(error "jump stack not empty"))
      (let ((c (string-ref program i)))
	(cond ((eq? c #\[)
	       (build_jumps_inner (+ 1 i) (cons i stack) forward backward))
	      ((eq? c #\])
	       (let ((open (car stack))
		     (close i)
		     (stack (cdr stack)))
		 (begin
		   (hash-table-set! forward open close)
		   (hash-table-set! backward close open)
		   (build_jumps_inner (+ 1 i) stack forward backward))))
	      (#t (build_jumps_inner (+ 1 i) stack forward backward))))))
  (build_jumps_inner 0 '() (make-hash-table) (make-hash-table)))
(define (jump table i)
  (if (hash-table-exists? table i)
    (+ 1 (hash-table-ref table i))
    (error "missing jump.")))

(define (tape-inc tape i)
  (if (< i 0)
    (error "negative index: " i)
    (u8vector-set! tape i (modulo (+ (u8vector-ref tape i) 1) 256))))
(define (tape-dec tape i)
  (if (< i 0)
    (error "negative index: " i)
    (u8vector-set! tape i (modulo (- (u8vector-ref tape i) 1) 256))))


(define (bf program)
  (define (bf_state pc ptr tape forward backward)
    (if (>= pc (string-length program))
      "done"
      (let ((c (string-ref program pc))
	    (next (+ 1 pc)))
	(cond ((eq? c #\+) (begin
			     (tape-inc tape ptr)
			     (bf_state next ptr tape forward backward)))
	      ((eq? c #\-) (begin
			     (tape-dec tape ptr)
			     (bf_state next ptr tape forward backward)))
	      ((eq? c #\>) (bf_state next (+ ptr 1) tape forward backward))
	      ((eq? c #\<) (bf_state next (- ptr 1) tape forward backward))
	      ((eq? c #\.) (begin
			     (display (integer->char (u8vector-ref tape ptr)))
			     (bf_state next ptr tape forward backward)))
	      ((eq? c #\[) (bf_state
			     (if (= 0 (u8vector-ref tape ptr))
			       (jump forward pc)
			       next)
			     ptr tape forward backward))
	      ((eq? c #\]) (bf_state
			     (if (= 0 (u8vector-ref tape ptr))
			       next
			       (jump backward pc))
			     ptr tape forward backward))
	      (#t (error "unknown instruction" c))))))
  (let ((jumps (build_jumps program)))
    (bf_state 0 0 (make-u8vector TAPE_LEN 0)
	      (car jumps)
	      (cdr jumps))))

(define (is-comment? line)
  (and (> (string-length line) 0)
       (eq? #\; (string-ref line 0))))

; given a list of lines,
; return a string representing the program.
; lines starting with ; are dropped
; newlines between lines are dropped.
(define (clean lines acc)
  (if (null? lines)
    acc
    (if (is-comment? (car lines))
      (clean (cdr lines) acc)
      (clean (cdr lines)
	     (string-append acc (car lines))))))

(define (main filename)
  (let* (
	 (file (open-input-file filename))
	 (program (clean (read-lines file) (string))))
    (bf program)))
(main (car (command-line-arguments)))
