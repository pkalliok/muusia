#lang racket

(require racket/set)
(require "muusat.rkt")
(provide sävelmä)

(define (yhdistä-muusat tilanne muusat)
  (if (empty? muusat) (values tilanne (set))
    (let*-values (((uusi-tilanne tapahtumat1) ((first muusat) tilanne))
		  ((lopputilanne tapahtumat2)
		   (yhdistä-muusat uusi-tilanne (rest muusat))))
      (values lopputilanne (set-union tapahtumat1 tapahtumat2)))))

(define (aloittava-isku? isku)
  (ormap (lambda (muusa) (or (sävel? muusa) (tauko? muusa))) isku))

(define (aikaa iskut)
  (cond ((empty? iskut) 0)
	((aloittava-isku? (first iskut)) 0)
	(else (add1 (aikaa (rest iskut))))))

(define (sävelmä nimi nopeus iskut)
  (procedure-rename
    (lambda (tilanne)
      (let lähtien ((tilanne tilanne) (iskut iskut))
	(if (empty? iskut) (values tilanne (set))
	  (let*-values (((pituus) (* nopeus (add1 (aikaa (rest iskut)))))
			((uusi-tilanne tapahtumat1)
			 (yhdistä-muusat (hash-set tilanne 'pituus pituus)
					 (map muusaksi (first iskut))))
			((lopputilanne tapahtumat2)
			 (lähtien (hash-update uusi-tilanne 'alku
					       (lambda (alku) (+ alku nopeus)))
				  (rest iskut))))
	    (values lopputilanne (set-union tapahtumat1 tapahtumat2))))))
    nimi))

