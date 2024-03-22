#lang racket

(require racket/set)
(provide uus-muus! iskujen-jako
	 oletustilanne alku loppu soitettavissa?
	 tapahtuma tapahtuma-hetki teos)

(define muusat (make-hash))

(define (uus-muus! muusa)
  (hash-set! (object-name muusa) muusa))

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
  (* (hash-ref tilanne 'aikamittakaava)
     (+ (hash-ref tilanne 'alku) (hash-ref tilanne 'aikasiirtymä))))

(define (loppu tilanne)
  (* (hash-ref tilanne 'aikamittakaava)
     (+ (hash-ref tilanne 'alku)
	(hash-ref tilanne 'pituus)
	(hash-ref tilanne 'aikasiirtymä))))

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
    (lambda (item) (hash-ref priorities item))))

(define (tapahtuma . args) (cons 1 args))
(define tapahtuma-hetki cadr)

(define tapahtuma-priority
  (compose
    (priority-function '("Header" "Start_track" "Title_t" "Tempo"
			 "Control_c" "Program_c" "Note_off_c" "Note_on_c"
			 "End_track" "End_of_file"))
    caddr))

(define (tapahtuma<? tap1 tap2)
  (or (< (tapahtuma-hetki tap1) (tapahtuma-hetki tap2))
      (and (= (tapahtuma-hetki tap1) (tapahtuma-hetki tap2))
	   (< (tapahtuma-priority tap1) (tapahtuma-priority tap2)))))

(define (teos nimi sisältö)
  (let*-values
    (((_ tapahtumajoukko) (sisältö oletustilanne))
     ((tapahtumat) (sort (set->list tapahtumajoukko) tapahtuma<?))
     ((loppuhetki) (if (empty? tapahtumat) 0
		     (+ (tapahtuma-hetki (last tapahtumat))
			(* 6 iskujen-jako)))))
    `((0 0 "Header" 0 1 ,iskujen-jako)
      (1 0 "Start_track")
      (1 0 "Title_t" ,nimi)
      (1 0 "Tempo" 500000)
      ,@tapahtumat
      (1 ,loppuhetki "End_track")
      (0 0 "End_of_file"))))
