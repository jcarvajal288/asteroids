(define-module (input)
               #:export (on-key-down))

(define key:up "ArrowUp")
(define key:down "ArrowDown")
(define key:left "ArrowLeft")
(define key:right "ArrowRight")

(define (on-key-down event)
  (let* ((key (keyboard-event-code event))
         (ship (level-ship *level*))
         (ship-vel (ship-velocity ship)))
      (if (string=? key key:left)
        (set-vec2-x! ship-vel (- (vec2-x ship-vel) thrust-accel)))
      (if (string=? key key:right)
        (set-vec2-x! ship-vel (+ (vec2-x ship-vel) thrust-accel)))
      (if (string=? key key:up)
        (set-vec2-y! ship-vel (- (vec2-y ship-vel) thrust-accel)))
      (if (string=? key key:down)
        (set-vec2-y! ship-vel (+ (vec2-y ship-vel) thrust-accel)))))
