#lang racket

(require racket/set)
(require "muusat.rkt")
(provide midisoitin)

(define (varaa-kanava tilanne soitin)
  (let ((soittimet (hash-ref tilanne 'soittimet)))
    (if (hash-has-key? soittimet soitin)
      (values tilanne (hash-ref soittimet soitin) (set))
      (let-values (((kanava) (set-first
			       (set-subtract
				 (list->set (range 16))
				 (list->set (hash-values soittimet)))))
		   ((msb lsb) (quotient/remainder (first soitin) 128)))
	(values (hash-update tilanne 'soittimet
			     (lambda (s) (hash-set s soitin kanava)))
		kanava
		(set (tapahtuma (alku tilanne) "Control_c" kanava 0 msb)
		     (tapahtuma (alku tilanne) "Control_c" kanava 32 lsb)
		     (tapahtuma (alku tilanne) "Program_c" kanava
				(second soitin))))))))

(define (midisoitin nimi pankki ohjelma)
  (let ((soitin (list pankki ohjelma)))
    (procedure-rename
      (lambda (tilanne)
	(if (not (soitettavissa? tilanne)) (values tilanne (set))
	  (let-values (((uusi-tilanne kanava soittimenvaihto)
			(varaa-kanava tilanne soitin)))
	    (values
	      uusi-tilanne
	      (set-union
		soittimenvaihto
		(set (tapahtuma (alku tilanne) "Note_on_c" kanava
				(hash-ref tilanne 'sävel)
				(hash-ref tilanne 'painotus))
		     (tapahtuma (loppu tilanne) "Note_off_c" kanava
				(hash-ref tilanne 'sävel)
				(hash-ref tilanne 'painotus))))))))
      nimi)))

