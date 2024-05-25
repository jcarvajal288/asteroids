(define-module (level)
  #:pure
  #:use-module (scheme base)
  #:use-module (math vector)
  #:use-module (math rect)
  #:use-module (ship)
  #:use-module (asteroid)
  #:export (make-default-level
            make-level
            level-width
            level-height
            level-ship
            level-asteroids
            destroy-ship!))

(define-record-type <level>
  (make-level width height ship asteroids)
  level?
  (width level-width)
  (height level-height)
  (ship level-ship level-ship-set!)
  (asteroids level-asteroids))

(define (make-default-level)
  (let* ((width 1280.0)
         (height 1024.0)
         (ship (make-default-ship width height))
         (asteroids (map (lambda (_) (build-asteroid width height)) (make-list 5 0))))
    (make-level width height ship asteroids)))

(define (destroy-ship! *level*)
  (level-ship-set! *level* 0))
