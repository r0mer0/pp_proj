#lang racket

;Define functions

(define pow
	(lambda (x n)
		(let pow_i ((i 0) (p 1))
			(cond			
				((= n i) p)
				((> n 0) (pow_i (+ i 1) (* p x)))
				(else (pow_i (- i 1) (/ p x)))))))


(define area_quadrado
  (lambda (a b)
    (* a b)))

(define area_paralelograma
  (lambda (b h)
    (* b h)))

(define area_retangulo
  (lambda (a b)
    (* a b)))

(define area_trapezio
  (lambda (h a b)
    (* 0.5(* h(+ b a)))))

(define area_poligono
  (lambda (a b)
    (* a b)))

(define area_triangulo
  (lambda (a b)
    (* a (* 0.5 b))))

(define area_circulo
  (lambda (r)
    (* r (* 3.14 3.14))))

(define volume_cubo
  (lambda (a b c)
    (* a (* b c))))

(define volume_cilindro
  (lambda (r h)
    (* 3.14 (* h (* r r)))))

(define volume_esfera
  (lambda (r)
  (* 0.3333333333333333333333333333333 (* 4 (* 3.14(* r r r))))))

(define volume_piramide
  (lambda (b h)
  (* 0.3333333333333333333333333333333(* b h))))

(define volume_cone
  (lambda (r h)
  (* 0.3333333333333333333333333333333 (* h (* 3.14(* r r))))))

(define fatorial
  (lambda (n)
    (if (= n 0) 1
        (* n (fatorial (- n 1))))))

(define seno
    (lambda (x)
      (sin x)))
  
(define cosseno
  (lambda (x)
    (cos x)))
  
(define tangente
  (lambda (x)
    (tan x)))

(define arco_seno
    (lambda (x)
      (asin x)))
  
(define  arco_cosseno
  (lambda (x)
    (acos x)))
  
(define  arco_tangente
  (lambda (x)
    (atan x)))


;TCP test server;
(require racket/tcp)
(define l (tcp-listen 5400))

(define-values (i o) (tcp-accept l))

(file-stream-buffer-mode i 'none)
(file-stream-buffer-mode o 'none)
(read i)
(display  "Bem-vindo ao UFAlphaBC. Entre com a operacao solicitada:" o)   
(close-input-port i)
(close-output-port o)
  
;Textos;

(define ajuda(string-append "--"))

