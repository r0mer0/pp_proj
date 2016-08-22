;import utilities
(use srfi-18)

(load "channel.scm")

(import channel)

;define utilities

(define (whoami)
  (thread-name (current-thread)))

(define (random-sleep)
  (thread-sleep! (* (random 100) 0.01)))

(define (make-request op arg1 arg2 resp)
  (lambda (m)
    (cond ((eq? m 'get-op) (lambda () op))
          ((eq? m 'get-arg1) (lambda () arg1))
          ((eq? m 'get-arg2) (lambda () arg2))
          ((eq? m 'get-resp) (lambda () resp))
          (else (error "Unknown method")))))

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
  (lambda (a)
    (* a (* a a))))

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

;Server;

(define (make-server)
  (define request (make-channel))
  (define (serve)
    (let loop ((req ((request 'receive))))
      (define response ((req 'get-resp)))
      (define op ((req 'get-op)))
      (define arg1 ((req 'get-arg1)))
      (define arg2 ((req 'get-arg2)))
      ;dispatcher
      (thread-start!
        (make-thread
          (cond ((eq? op 'pow)
            (lambda ()
              (print "Processamento lento...")
              (pow_server response arg1 arg2)
              (random-sleep)))              
            ((eq? op 'area_quadrado)
              (lambda ()
                (area_quadrado_server response arg1 arg2)))
            ((eq? op 'area_retangulo)
              (lambda ()
                (area_retangulo_server response arg1 arg2)))
            ((eq? op 'area_circulo)
              (lambda ()
                (area_circulo_server response arg1)))
            ((eq? op 'area_trapezio)
              (lambda ()
                (area_trapezio_server response arg1 arg2)))
            ((eq? op 'area_triangulo)
              (lambda () 
                (area_triangulo_server response arg1 arg2)))
            ((eq? op 'area_paralelograma)
              (lambda ()
                (area_paralelograma_server response arg1 arg2)))
            ((eq? op 'volume_cubo)
              (lambda ()
                (volume_cubo_server response arg1)))
            ((eq? op 'volume_cilindro)
              (lambda ()
                (volume_cilindro_server response arg1 arg2)))
            ((eq? op 'volume_esfera)
              (lambda ()
                (volume_esfera_serve response arg1)))
            ((eq? op 'volume_cone)
              (lambda ()
                (volume_cone_server response arg1 arg2)))
            ((eq? op 'fatorial)
              (lambda ()
                (fatorial_server response arg1)))
            ((eq? op 'seno)
              (lambda ()
                (seno_server response arg1)))
            ((eq? op 'cosseno)
              (lambda ()
                (cosseno_server response arg1)))
            ((eq? op 'tangente)
              (lambda ()
                (tangente_server response arg1)))
            ((eq? op 'arco_seno)
              (lambda ()
                (arco_seno_server response arg1)))
            ((eq? op 'arco_cosseno)
              (lambda ()
                (arco_cosseno_server response arg1)))
            ((eq? op 'arco_tangente)
              (lambda ()
                (arco_tangente_server response arg1)))
            ((eq? op 'derivada)
              (lambda ()
                (derivada_server response arg1 arg2)))
            ((eq? op '+)
              (lambda ()
                (plus response arg1 arg2)))
            ((eq? op '-)
              (lambda ()
                (minus response arg1 arg2)))
            ((eq? op '/)
              (lambda ()
                (divide response arg1 arg2)))
            ((eq? op '*)
              (lambda ()
                (multiply response arg1 arg2)))
            (else (lambda () (error response))))))
      (loop ((request 'receive)))))
  
  (define (get-request)
    request)

  (define (pow_server response arg1 arg2)
    ((response 'send) (pow arg1 arg2)))  

  (define (area_quadrado_server response arg1 arg2)
    ((response 'send) (area_quadrado arg1 arg2)))

  (define (area_paralelograma_server response arg1 arg2)
    ((response 'send) (area_paralelograma arg1 arg2)))

  (define (area_retangulo_server response arg1 arg2)
    ((response 'send) (area_retangulo arg1 arg2)))

  (define (area_trapezio_server response arg1 arg2)
    ((response 'send) (area_trapezio arg1 arg2)))

  (define (area_triangulo_server response arg1 arg2)
    ((response 'send) (area_triangulo arg1 arg2)))

  (define (area_circulo_server response arg1)
    ((response 'send) (area_circulo arg1)))

  (define (volume_cubo_server response arg1)
    ((response 'send) (volume_cubo arg1))) 

  (define (volume_cilindro_server response arg1 arg2)
    ((response 'send) (volume_cilindro arg1 arg2)))

  (define (volume_esfera_serve response arg1)
    ((response 'send) (volume_esfera arg1)))

  (define (volume_cone_server response arg1 arg2)
    ((response 'send) (volume_cone arg1 arg2)))

  (define (fatorial_server response arg1)
    ((response 'send) (fatorial arg1)))

  (define (seno_server response arg1)
    ((response 'send) (seno arg1)))

  (define (cosseno_server response arg1)
    ((response 'send) (cosseno arg1)))

  (define (tangente_server response arg1)
    ((response 'send) (tangente arg1)))

  (define (arco_seno_server response arg1)
    ((response 'send) (arco_seno arg1)))

  (define (arco_cosseno_server response arg1)
    ((response 'send) (arco_cosseno arg1)))

  (define (arco_tangente_server response arg1)
    ((response 'send) (arco_tangente arg1)))

  (define (derivada_server response arg1 arg2)
    ((response 'send) (derivada arg1 arg2)))

  (define (plus response arg1 arg2)
    ((response 'send) (+ arg1 arg2)))
  
  (define (minus response arg1 arg2)
    ((response 'send) (- arg1 arg2)))
  
  (define (divide response arg1 arg2)
    ((response 'send) (/ arg1 arg2)))
  
  (define (multiply response arg1 arg2)
    ((response 'send) (* arg1 arg2)))  

  (define (error response)
    ((response 'send) "Unknown operation"))

  (lambda (m)
    (cond ((eq? m 'serve) serve)
          ((eq? m 'get-request) get-request)
          (else error "Unknown method"))))

;Client
(define (make-client request op)
  (define response (make-channel))
  (define (send-sequence start end)
    (let loop ((i start))
      (if (< i end)
        (begin
          ((request 'send) (make-request op i (+ i 1) response))
          (print (whoami) '__ i op (+ i 1) '= ((response 'receive)))
          (loop (+ i 1))))))
  (lambda (m)
    (cond ((eq? m 'send-sequence) send-sequence)
          (else (error "Unknown method")))))

(define server (make-server))
(define request ((server 'get-request)))

(define t-s (make-thread (lambda () ((server 'serve))) 's))
(define t-c1 (make-thread 
               (lambda () 
                 (define client (make-client request 'pow))
                 ((client 'send-sequence) 0 10)) 'c-1))
(define t-c2 (make-thread 
               (lambda () 
                 (define client (make-client request 'area_quadrado))
                 ((client 'send-sequence) 10 20)) 'c-2))
(define t-c3 (make-thread 
               (lambda () 
                 (define client (make-client request 'volume_cone))
                 ((client 'send-sequence) 20 30)) 'c-3))
(define t-c4 (make-thread 
               (lambda () 
                 (define client (make-client request '*))
                 ((client 'send-sequence) 30 40)) 'c-4))

(map thread-start! (list t-s t-c1 t-c2 t-c3 t-c4))
;(thread-sleep! 120)                                            

;Textos;

(define ajuda(string-append 
 "Potência: (pow x n) sendo x a base e o n o expoente.  \n"
 "Área Quadrado: (area_quadrado b h) sendo b a base e h a altura.  " 
 "Área Paralelograma: (area_paralelograma b h) sendo b a base e h a altura. "
 "Área Retângulo: (area_retangulo b h) sendo b a base  e h a altura. "
 "Área Trapézio: (area_trapezio h b1 b2) sendo b2 a base menor, b2 a base maior e h a altura. "
 "Área Triângulo: (area_triangulo h b1 b2) sendo b a base  e h a altura. "
 "Volume Cubo: (volume_cubo a) sendo  a o valor de cada aresta. "
 "Volume Cilindro: (volume_cilindro r h) sendo r o raio e h a altura. "
 "Volume Esfera: (volume_esfera r ) sendo r o raio. "
 "Fatorial: (fatorial n) "
 "Seno: (seno n) "
 "Cosseno: (cosseno n) "
 "Tangente: (tangente n) "
 "Arco Seno: (arco_seno n) "
 "Arco Cosseno: (arco_cosseno n) "
 "Arco Tangente: (arco_tangente n) "
 "Derivada: (derivada e v) sendo e a expressão e v a variavel. "
 ))
