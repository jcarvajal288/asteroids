(define-module (types)
  #:pure
  #:use-module (scheme base)
  #:use-module (hoot ffi)
  #:use-module (math vector)
  #:use-module (math rect)
  #:use-module (ship)
  #:export (dt
            make-default-level
            make-level
            level-width
            level-height
            level-ship))

(define dt (/ 1000.0 60.0)) ; aim for updating at 60Hz

(define-record-type <level>
  (make-level width height ship)
  level?
  (width level-width)
  (height level-height)
  (ship level-ship))

(define (make-default-level)
  (let* ((width 1280.0)
         (height 1024.0)
         (ship (make-default-ship width height)))
    (make-level width height ship)))


