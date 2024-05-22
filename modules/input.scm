(define-module (input)
  #:pure
  #:use-module (scheme base)
  #:use-module (scheme write)
  #:use-module (hoot ffi)
  #:use-module (dom event)
  #:use-module (math vector)
  #:use-module (types)
  #:export (on-key-down
            on-key-up 
            command:move-left
            command:move-right
            command:move-forward
            command:move-backward))

(define key:up "ArrowUp")
(define key:down "ArrowDown")
(define key:left "ArrowLeft")
(define key:right "ArrowRight")

(define command:move-left #f)
(define command:move-right #f)
(define command:move-forward #f)
(define command:move-backward #f)

(define (on-key-down event)
  (display "on key down")
  (let ((key (keyboard-event-code event)))
    (cond ((string=? key key:left)  (set! command:move-left #t)) 
          ((string=? key key:right) (set! command:move-right #t)) 
          ((string=? key key:up)    (set! command:move-forward #t))
          ((string=? key key:down)  (set! command:move-backward #t)))))

(define (on-key-up event)
  (let ((key (keyboard-event-code event)))
    (cond ((string=? key key:left)  (set! command:move-left #f)) 
          ((string=? key key:right) (set! command:move-right #f)) 
          ((string=? key key:up)    (set! command:move-forward #f))
          ((string=? key key:down)  (set! command:move-backward #f)))))
