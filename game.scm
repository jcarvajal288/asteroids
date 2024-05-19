(import (scheme base)
        (scheme inexact)
        (hoot debug)
        (hoot ffi)
        (hoot hashtables)
        (hoot match)
        (dom canvas)
        (dom document)
        (dom element)
        (dom event)
        (dom image)
        (dom media)
        (dom window)
        (math)
        (math rect)
        (math vector))

(define-record-type <ship>
  (make-ship velocity hitbox)
  ship?
  (velocity ship-velocity)
  (hitbox ship-hitbox))

(define-record-type <level>
  (make-level ship)
  level?
  (ship level-ship))

(define image:ship (make-image "assets/images/ship-1.png"))

(define game-width    1280.0)
(define game-height   1024.0)
(define ship-width    43.0)
(define ship-height   41.0)
(define thrust-accel  1.0)

(define (make-level-1)
  (make-level (make-ship (vec2 0.0 0.0)
                         (make-rect (- (/ game-width 2.0) (/ ship-width 2)) 
                               (- (/ game-height 2.0) (/ ship-height 2))
                               ship-width 
                               ship-height))))

(define *level* (make-level-1))

(define dt (/ 1000.0 60.0)) ; aim for updating at 60Hz
(define (update)
  (let* ((ship (level-ship *level*))
         (ship-hitbox (ship-hitbox ship))
         (ship-velocity (ship-velocity ship)))
    (set-rect-x! ship-hitbox (+ (rect-x ship-hitbox) (vec2-x ship-velocity)))
    (set-rect-y! ship-hitbox (+ (rect-y ship-hitbox) (vec2-y ship-velocity))))
  (timeout update-callback dt))
(define update-callback (procedure->external update))

(define (draw-background)
    (set-fill-color! context "#140c1c")
    (fill-rect context 0.0 0.0 game-width game-height))

(define (draw-ship)
  (let ((ship (level-ship *level*)))
    (draw-image context image:ship
                0.0 0.0 ship-width ship-height
                (rect-x (ship-hitbox ship)) (rect-y (ship-hitbox ship)) ship-width ship-height)))

(define (draw prev-time)
  (draw-background)
  (draw-ship)
  (request-animation-frame draw-callback))
(define draw-callback (procedure->external draw))

;; Input
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

;; Canvas and event loop setup
(define canvas (get-element-by-id "canvas"))
(define context (get-context canvas "2d"))
(set-element-width! canvas (exact game-width))
(set-element-height! canvas (exact game-height))
(add-event-listener! (current-document) "keydown"
                     (procedure->external on-key-down))
(request-animation-frame draw-callback)
(timeout update-callback dt)
