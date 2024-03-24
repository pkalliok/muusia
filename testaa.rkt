#lang racket

(require "muusat.rkt" "soitin.rkt" "savelma.rkt")

(define (alusta!) (uus-muus! (midisoitin 'hanuri 0 21) '(sävel)))
(define (test1) (teos (midisoitin 'jousi 0 40)))
(define (test2) (teos (sävelmä 'tunturi 1 '((hanuri) () (hanuri)))))

