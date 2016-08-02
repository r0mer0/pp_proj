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

(define area_triangulo
  (lambda (a b)
    (* a (/ 2 b))))

(define area_circulo
  (lambda (r)
    (* r (* 3.14 3.14))))

(define volume_cubo
  (lambda (a b c)
    (* a (* b c))))
;TCP test server;
(require racket/tcp)
(define l (tcp-listen 5400))

(define-values (i o) (tcp-accept l))

(file-stream-buffer-mode i 'none)
(file-stream-buffer-mode o 'none)
(read i)
(write "Bem-vindo ao UFAlphaBC. Entre com a operacao solicitada:" o)   
(close-input-port i)
(close-output-port o)



