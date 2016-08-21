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

(define (derivada d var)
 (cond ((number? d) 0)
 ((symbol? d)
 (if (eq? d var) 1 0))
 ((soma? d) 
 (make-soma (derivada (cadr d) var)
 (derivada (caddr d) var)))
 ((produto? d)
 (make-soma
 (make-produto (cadr d)
 (derivada (caddr d) var))
 (make-produto (derivada (cadr d) var)
 (caddr d))))
 ((pow-der? d)
 (if (eq? (cadr d) var)
 (make-produto (caddr d)
 (make-pow-der (cadr d) (- (caddr d) 1)))
 0))
 (else (error "Expressão inválida"))))

(define (pow-der? d)
 (and (pair? d) (eq? (car d) '^) (symbol? (cadr d))
 (number? (caddr d))))
(define (make-pow-der d1 d2)
 (cond ((= d2 0) 1)
 ((= d2 1) d1)
 (else (list '^ d1 d2))))

(define (soma? x)
(and (pair? x) 
   (eq? (car x) '+)))

(define (make-soma  . args)
  (cons '+ (apply append 
  (map 
  (lambda (x) (if (soma? x) 
  (cdr x) (list x))) args))))


(define (make-produto m1 m2) 
(list '* m1 m2))

(define (produto? x)
(and (pair? x) 
   (eq? (car x) '*)))

(define (multiplicador a) 
(cadr a))

(define (multiplicando a) 
(caddr a))

;TCP test server;
(require racket/tcp)
(define l (tcp-listen 5400))

(define-values (i o) (tcp-accept l))

(file-stream-buffer-mode i 'none)
(file-stream-buffer-mode o 'none)
(read i)
(write  "Bem-vindo ao UFAlphaBC. Entre com a operacao solicitada:"  o)   
(close-input-port i)
(close-output-port o)
  
;Textos;

(define ajuda(string-append 
 "Potência: (pow x n) sendo x a base e o n o expoente.  "
 "Área Quadrado: (area_quadrado b h) sendo b a base e h a altura.  " 
 "Área Paralelograma: (area_paralelograma b h) sendo b a base e h a altura. "
 "Área Retângulo: (area_retangulo b h) sendo b a base  e h a altura. "
 "Área Trapézio: (area_trapezio h b1 b2) sendo b2 a base menor, b2 a base maior e h a altura. "
 "Área Triângulo: (area_triangulo h b1 b2) sendo b a base  e h a altura. "
 "Volume Cubo: (volume_cubo a1 a2 a3) sendo  a o valor de cada aresta. "
 "Volume Cilindro: (volume_cilindro r h) sendo r o raio e h a altura. "
 "Volume Esfera: (volume_esfera r ) sendo r o raio. "
 "Fatorial: (fatorial n) "
 "Seno: (seno n) "
 "Cosseno: (cosseno n) "
 "Tangente: (tangente n) "
 "Arco Seno: (arco_seno n) "
 "Arco Cosseno: (arco_cosseno n) "
 "Arco Tangente: (arco_tangente n) "
 "Derivada: (derivada e v) sendo e a expressão e v o valor. "
 ))
