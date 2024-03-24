#lang racket

(require "muusat.rkt" "soitin.rkt" "savelma.rkt")

(uus-muus! (midisoitin 'hanuri 0 30))
(define (test1) (teos 'esimerkki (midisoitin 'jousi 0 40)))
(define (test2) (teos 'piippaus (sävelmä 'tunturi 1 '((hanuri) () (hanuri)))))

