#lang racket
;TCP Client test;
(require racket/tcp)

(define-values (i o) (tcp-connect "localhost" 5400))

(file-stream-buffer-mode i 'none)
(file-stream-buffer-mode o 'none)
(read i)
(write "Good Bye" o)
