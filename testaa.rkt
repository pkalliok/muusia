#lang racket

(require "muusat.rkt" "soitin.rkt" "savelma.rkt" "savel.rkt")

(define (alusta!)
  (uus-muus! (midisoitin 'hanuri 0 21) '(sävel))
  (luo-manner-sävelet!)
  (luo-vaihda-soitin!))

(define (test1) (teos (midisoitin 'jousi 0 40)))
(define (test2) (teos (sävelmä 'tunturi 1 '((hanuri) () (hanuri)))))
(define (test3) (teos (sävelmä 'hippula 1 '(((soitin hanuri) c) (d) (eb)))))

