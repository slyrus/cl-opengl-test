
(in-package :cl-opengl-test)

(on-initial-thread (:wait nil)
  (let ((image (opticl:make-8-bit-rgb-image 256 257)))
    (opticl:draw-rectangle image 50 50 200 200 255 0 0)
    (glut:display-window (make-opticl-window image))))


(on-initial-thread (:wait nil)
  (let ((image (opticl:read-image-file
                (asdf:system-relative-pathname 'opticl-test "images/truck.tiff"))))
    (opticl:draw-rectangle image 50 50 200 200 255 0 0)
    (glut:display-window (make-opticl-window image))))

