#lang racket

(require "muusat.rkt")
(provide tilanne->midikorkeus sävel luo-manner-sävelet!)

(define pohjat
  #hash((C . -12) (D . -10) (E . -8) (F . -7)
	(G . -5) (A . -3) (B . -2) (H . -1)
	(c . 0) (d . 2) (e . 4) (f . 5)
	(g . 7) (a . 9) (b . 10) (h . 11)))

(define muunne->puolisävelaskeleet
  (curry hash-ref #hash(("=" . 0) ("#" . 1) ("b" . -1)
				  ("+" . 12) ("-" . -12))))

(define (tilanne->midikorkeus tilanne)
  (let ((sävel (hash-ref tilanne 'sävel)))
    (+ (hash-ref tilanne 'sävelpohja)
       (hash-ref pohjat (first sävel))
       (foldl + 0 (map muunne->puolisävelaskeleet (rest sävel))))))

(define (sävel pohja muunteet)
  (procedure-rename
    (lambda (tilanne)
      ((muusaksi (hash-ref tilanne 'soitin))
       (hash-set tilanne 'sävel (cons pohja muunteet))))
    (string->symbol (apply string-append (symbol->string pohja) muunteet))))

(define (luo-manner-sävelet!)
  (for* ((pohja (hash-keys pohjat))
	 (muunteet '(() ("=") ("#") ("b") ("#" "#") ("b" "b")))
	 (oktaavimuunteet '(() ("-") ("-" "-") ("+") ("+" "+"))))
	(uus-muus! (sävel pohja (append muunteet oktaavimuunteet)))))

