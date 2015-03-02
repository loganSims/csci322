#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;
;;Logan Sims
;;03/01/2015
;;hw05 a
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;
(define A-channel (make-channel))
(define B-channel (make-channel))
(define count 0)

(define A-arrive (lambda ()
   (channel-get A-channel) 
   (channel-get A-channel)
   (channel-put B-channel #t)
   (channel-put B-channel #t)))

(define B-arrive (lambda ()
   (channel-put A-channel #t)
   (channel-get B-channel)))

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

(define thread-B2
(thread
  (lambda ()
            (let loop ()
              (displayln "B2 arrives")
              (B-arrive)
              (displayln "B2 passes")
              (loop)))))

;main loop 
;stops program after A recives a certain number
;of messages
(let loop ()  
  (if (equal? count 10) 
      (begin (thread-suspend thread-A)
             (thread-suspend thread-B1)
             (thread-suspend thread-B2)) 
      (loop)))