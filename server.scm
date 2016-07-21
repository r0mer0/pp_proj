#lang racket
;TCP test server;
(require racket/tcp)
(define l (tcp-listen 5400))

(define-values (i o) (tcp-accept l))

(file-stream-buffer-mode i 'none)
(file-stream-buffer-mode o 'none)

(write "hi" o)
(read i)
(close-input-port i)
(close-output-port o)



