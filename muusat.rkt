#lang racket

(require racket/set)
(provide uus-muus! muusaksi sävel? tauko?
	 iskujen-jako oletustilanne alku loppu soitettavissa?
	 tapahtuma tapahtuma-hetki teos)

(define muusat (make-hash))
(define muusattimet (make-hash))
(define sävelet (mutable-set))

(define (muusaksi juttu)
  (cond ((procedure? juttu) juttu)
	((hash-ref muusat juttu #f) => identity)
	((and (list? juttu) (not (empty? juttu))
	      (hash-ref muusattimet (first juttu) #f))
	 => (lambda (muusatin) (apply muusatin (rest juttu))))
	((symbol? juttu)
	 (let* ((osat (string-split (symbol->string juttu) ":"))
		(nimi (string->symbol (first osat)))
		(muusatin (hash-ref muusattimet nimi #f)))
	   (if (not muusatin) (error "Tuntematon muusatin" nimi)
	     (apply muusatin (rest osat)))))
	(else (error "Tuntematon muusa" juttu))))

(define sävel? (curry set-member? sävelet))
(define tauko? (const #f))

(define (uus-muus! muusa (luokat '()))
  (let ((nimi (object-name muusa)))
    (hash-set! (if (memq 'muusatin luokat) muusattimet muusat) nimi muusa)
    (when (memq 'sävel luokat) (set-add! sävelet nimi))))

(define iskujen-jako 360)

(define oletustilanne
  (make-immutable-hash
    `((tyyppi . tilanne)
      (alku . 0)
      (pituus . 4)
      (aikamittakaava . ,iskujen-jako)
      (aikasiirtymä . 0)
      (soitin . ,(lambda (tilanne) (values tilanne (set))))
      (keskeytykset . ())
      (soittimet . #hash())
      (sävel . 64)
      (painotus . 100))))

(define (alku tilanne)
  (exact-round
    (* (hash-ref tilanne 'aikamittakaava)
       (+ (hash-ref tilanne 'alku) (hash-ref tilanne 'aikasiirtymä)))))

(define (loppu tilanne)
  (exact-round
    (* (hash-ref tilanne 'aikamittakaava)
       (+ (hash-ref tilanne 'alku)
	  (hash-ref tilanne 'pituus)
	  (hash-ref tilanne 'aikasiirtymä)))))

(define (soitettavissa? tilanne)
  (let ((alku (hash-ref tilanne 'alku)))
    (not (ormap (lambda (keskeytys)
		  (and (<= (first keskeytys) alku)
		       (< alku (second keskeytys))))
		(hash-ref tilanne 'keskeytykset)))))

(define (priority-function priority-list)
  (let ((priorities (make-immutable-hash
		      (map cons priority-list
			   (range (length priority-list))))))
    (curry hash-ref priorities)))

(define (tapahtuma . args) (cons 1 args))
(define tapahtuma-hetki cadr)

(define tapahtuma-priority
  (compose
    (priority-function '(Header Start_track Title_t Tempo
			 Note_off_c Control_c Program_c Note_on_c
			 End_track End_of_file))
    caddr))

(define (tapahtuma<? tap1 tap2)
  (or (< (tapahtuma-hetki tap1) (tapahtuma-hetki tap2))
      (and (= (tapahtuma-hetki tap1) (tapahtuma-hetki tap2))
	   (< (tapahtuma-priority tap1) (tapahtuma-priority tap2)))))

(define (teos sisältö)
  (let*-values
    (((_ tapahtumajoukko) (sisältö oletustilanne))
     ((tapahtumat) (sort (set->list tapahtumajoukko) tapahtuma<?))
     ((loppuhetki) (if (empty? tapahtumat) 0
		     (+ (tapahtuma-hetki (last tapahtumat))
			(* 6 iskujen-jako)))))
    `((0 0 Header 0 1 ,iskujen-jako)
      (1 0 Start_track)
      (1 0 Title_t ,(object-name sisältö))
      (1 0 Tempo 500000)
      ,@tapahtumat
      (1 ,loppuhetki End_track)
      (0 0 End_of_file))))

