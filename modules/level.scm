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
            level-score
            level-asteroids
            level-asteroids-set!
            level-missiles
            level-missiles-set!
            add-score-for-asteroids))

(define-record-type <level>
  (make-level width height ship score asteroids missiles)
  level?
  (width level-width)
  (height level-height)
  (ship level-ship level-ship-set!)
  (score level-score level-score-set!)
  (asteroids level-asteroids level-asteroids-set!)
  (missiles level-missiles level-missiles-set!))

(define (make-default-level)
  (let* ((width 1280.0)
         (height 1024.0)
         (ship (make-default-ship width height))
         (score 0)
         (asteroids (map (lambda (_) (build-asteroid width height)) (make-list 10 0)))
         (missiles '()))
    (make-level width height ship score asteroids missiles)))

(define (add-score-for-asteroids *level* asteroids)
  (let ((new-points (apply + (map asteroid-score-value asteroids))))
    (level-score-set! *level* (+ (level-score *level*) new-points))))

