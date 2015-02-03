#lang racket
;; Geoffrey Matthews
;; 2013
;; A non-threaded planet demo
;;;;;;;;;;;;;;;;;;;;
;;Edits: Logan Sims
;;CSCI 322
;;Winter 2015      
;;;;;;;;;;;;;;;;;;;;

(require racket/gui)

;;;;;;;;;;;;;;;added code;;;;;;;;;;;;;;;;;;
(define turnstile1 (make-semaphore 0))
(define turnstile2 (make-semaphore 1))
(define mutex (make-semaphore 1))
(define counter 0)
(define moveRoomEmpty (make-semaphore 1))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Small 2d vector library for the Newtonian physics
(define (x v) (vector-ref v 0))
(define (y v) (vector-ref v 1))
(define (x! v value) (vector-set! v 0 value))
(define (y! v value) (vector-set! v 1 value))
(define (v* v value) (vector-map (lambda (x) (* x value)) v))
(define (v+ v w) (vector-map + v w))
(define (v- v w) (vector-map - v w))
(define (v-zero! v) (vector-map! (lambda (x) 0) v))
(define (v-dot v w) (let ((vw (vector-map * v w))) (+ (x vw) (y vw))))
(define (v-mag v) (sqrt (v-dot v v)))

;; Planet object
(define planet%
  (class object%
    (public m p v calculate-force move draw)
    (init-field (mass 1)
                (position (vector 0 0 ))
                (velocity (vector 0 0 ))
                (force (vector 0 0 ))
;;;;;;;;;;;;;;;;;;;;;;;added code;;;;;;;;;;;;;;;;;
;a control thread is now a feild of each planet
                (t (thread
                     (lambda ()
                       (let loop ()
;Want all planets to calculate force before they can (move)  
                         (semaphore-wait mutex) 
                         (set! counter (+ counter 1))
                         (cond ((= counter (send planet-container num-planets))
                                    (semaphore-wait turnstile2)
                                    (semaphore-post moveRoomEmpty);unlocks room so screen can refresh
                                    (semaphore-post turnstile1)))
                         (semaphore-post mutex)
                         
                         (semaphore-wait turnstile1)
                         (semaphore-post turnstile1)
                         
                         ;critical point
                         (calculate-force (send planet-container get-planets))
                         
                         (semaphore-wait mutex) 
                         (set! counter (- counter 1))
                         (cond ((= counter 0)
                                    (semaphore-wait turnstile1)
                                    (semaphore-wait moveRoomEmpty);locks room while planets move
                                    (semaphore-post turnstile2)))
                         (semaphore-post mutex)
                         
                         (semaphore-wait turnstile2)
                         (semaphore-post turnstile2)
                         ;critical point
                         (move)
 
                         (sleep .05)
                       (loop))))))
    (thread-suspend t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (m) mass)
    (define (p) position)
    (define (v) velocity)
    ;; Use Newton's law of gravitation.
    ;; I assume the gravitational constant is one
    (define (calculate-force pl)
      (v-zero! force)
      (for-each (lambda (other-planet)
                  (when (not (equal? this other-planet))
                    (let* ((direction (v- (send other-planet p) position))
                           (dist (max 1 (v-mag direction)))
                           (other-mass (send other-planet m))
                           (new-force (v* direction (/ (* mass other-mass) (* dist dist))))
                          )
                      (vector-map! + force new-force))))
                pl)
      )
    ;; Simple Euler integration of acceleration and velocity
    (define (move) 
      (let ((acc (v* force (/ 1.0 mass))))
        (vector-map! + velocity acc)
        (vector-map! + position velocity)))
    ;; Draw a circle 
    (define (draw dc) 
      (send dc set-brush brush)
      (send dc set-pen pen)
      (send dc draw-ellipse (x position) (y position) radius radius ))
    ;; Initialize 
    ;(x! velocity (* 2 (- 0.5 (random))))
    ;(y! velocity (* 2 (- 0.5 (random))))
    (set! mass (+ 1 (* 10 (random))))
    (define radius (* 5 (sqrt mass)))
    (define color 
      (let* ((r (random))
             (b (real->floating-point-bytes r 4)))
        (make-object color% (bytes-ref b 0) (bytes-ref b 1) (bytes-ref b 2) )))
    (define brush (make-object brush% color))
    (define pen (make-object pen% color))
    ;; Don't forget the super-new!
    (super-new)
    ))
;; Abstract the list-handling for a list of planets
(define planet-container%
  (class object%
    (public add-planet calculate-force move draw get-planets reset resume suspend kill num-planets)
    (init-field (planets '()))
    (define (get-planets) planets)
    (define (reset) (set! planets '()))
    (define (add-planet planet)
      (set! planets (cons planet planets)))
;;;;;;;;;;;;;;;;;;;added code;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (num-planets) (length planets))
;control functions for planet threads    
    (define (suspend)
      (for-each (lambda (planet)
                  (thread-suspend (get-field t planet)))
                planets))
    
    (define (resume)
      (for-each (lambda (planet)
                  (thread-resume (get-field t planet)))
                planets))
    
    (define (kill)
      (for-each (lambda (planet)
                  (kill-thread (get-field t planet))
                  (remv planet planets))
                planets))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (calculate-force)
      (for-each (lambda (planet)
                  (send planet calculate-force planets))
                planets))
    (define (move)
      (for-each (lambda (planet)
                  (send planet move))
                planets))
    (define (draw dc)
      (for-each (lambda (planet)
                  (send planet draw dc))
                planets))
    (super-new)
    )
  )
(define planet-container (new planet-container%))
    
;; The GUI
;;;;;;;;;;;;;;;;;;;;;;added code;;;;;;;;;;;;;;;;;;;
;kills animate and all planet threads
(define my-frame%
  (class frame%
    (define (on-close)
      (send planet-container kill)
      (kill-thread animate))
    (augment on-close)
    (super-new)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define frame (new my-frame% 
                   (label "Planets")
                   (min-width 120)
                   (min-height 80)
                   ))
(send frame create-status-line)
(send frame show #t)

(define h-panel
  (new horizontal-panel%
       (parent frame)
       (stretchable-height #f)
       (style '(border))
       (border 2)))

(define run-checkbox
  (new check-box%
       (parent h-panel)
       (label "Run animation")
;;;;;;;;;;;;;;;;;;;;;;added code;;;;;;;;;;;;;;;;;;;
;suspends/resumes animate thread and all planet threads
       ;callback instead of busy loop
       (callback 
        (lambda (button event)
          (cond((send run-checkbox get-value)
               (thread-resume animate) 
               (send planet-container resume))
          (else 
               (thread-suspend animate)
               (send planet-container suspend)))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define reset-button
  (new button%
       (parent h-panel)
       (label "Reset")
       (callback
        (lambda (b e)
          (send planet-container kill)
          (send planet-container reset)
          (set! counter 0)
          (send canvas refresh)))))

(define my-canvas%
  (class canvas%
    (override on-paint on-event)
    (define (on-paint)
      (let ((dc (send this get-dc))
            (w (send this get-width))
            (h (send this get-height)))
        (send dc clear)
        (send planet-container draw dc)
        ))
    (define (on-event event)
      (when (send event button-down?)
        (let ((x (send event get-x))
              (y (send event get-y)))
          (send frame set-status-text (format "Mouse at ~a ~a" x y))
          (send planet-container add-planet (new planet% (position (vector x y))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;checks if planet should be resumed when it is created
          (cond((send run-checkbox get-value)
               (send planet-container resume)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          (send this refresh)))
      )
    (super-new)
    (send (send this get-dc) set-background (make-object color% 8 8 64))
    ))

(define canvas
  (new my-canvas%
       (parent frame)
       (style '(border))
       (min-width 640)
       (min-height 480)))

;;;;;;;;;;;added code;;;;;;;;;;;;;;;;;;;;;;
;animate thread instead of busy loop
(define animate
 (thread  
   (lambda ()
     (let loop ()
       
       (semaphore-wait moveRoomEmpty)
       (send canvas refresh)
       (semaphore-post moveRoomEmpty)
       
       (sleep .05)
       (loop)))))
(thread-suspend animate)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;