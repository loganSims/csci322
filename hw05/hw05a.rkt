#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;
;;Logan Sims
;;03/01/2015
;;hw05 b
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;
(define channel (make-channel))
(define count 0)

(define A-arrive (lambda ()
   (channel-get channel) 
   (channel-get channel)))

(define B-arrive (lambda ()
   (channel-put channel #t)))

(define thread-A
(thread
  (lambda ()
            (let loop ()
              (displayln "A arrives")
              (A-arrive)
              (displayln "A passes")
              (set! count (+ count 1))
              (loop)))))

(define thread-B1
(thread
  (lambda ()
            (let loop ()
              (displayln "B1 arrives")
              (B-arrive)
              (displayln "B1 passes")
              (loop)))))


;main loop 
;stops program after A recives a certain number
;of messages
(let loop ()
  (if (equal? count 1) 
      (begin (thread-suspend thread-A)
             (thread-suspend thread-B1)) 
      (loop)))
