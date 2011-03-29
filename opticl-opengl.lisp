
(in-package :cl-opengl-test)

(defclass opticl-window (glut:window)
  ((texture :accessor texture)
   (framebuffer :accessor framebuffer)
   (image :accessor image :initarg :image)
   (dirty :accessor dirty))
  (:default-initargs :image nil :height 128 :width 128
                     :dirty nil
                     :title "Opticl Image Viewer"
                     :pos-x 100 :pos-y 100))

(defun make-opticl-window (image)
  (make-instance 'opticl-window
                 :height (array-dimension image 0)
                 :width (array-dimension image 1)
                 :image image))

(defun draw (window)
  (gl:load-identity)
  (gl:bind-framebuffer-ext :framebuffer-ext 0)
  (gl:clear-color 0 0 0.2 0)
  (gl:clear-depth 1)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (with-accessors ((image image)) window
    (opticl:with-image-bounds (height width) image
      (gl:viewport (max 0 (/ (- (glut:width window) width) 2))
                   (max 0 (/ (- (glut:height window) height) 2))
                   width height)
      (let ((texture (texture window)))  
        (gl:bind-texture :texture-2d texture)
        (gl:enable :texture-2d)
        (gl:with-primitives :quads
          (gl:tex-coord 0 0) (gl:vertex -1 1)
          (gl:tex-coord 1 0) (gl:vertex 1 1)
          (gl:tex-coord 1 1) (gl:vertex 1 -1)
          (gl:tex-coord 0 1) (gl:vertex -1 -1))
        (glut:swap-buffers)))))

(defmethod glut:display-window :before ((window opticl-window))
  (let ((texture (first (gl:gen-textures 1))))
    (gl:load-identity)
    (gl:bind-texture :texture-2d texture)
    (with-accessors ((image image)
                     (window-width glut:width)
                     (window-height glut:height))
        window
      (setf (texture window) texture)
      (opticl:with-image-bounds (image-height image-width channels) image
        (gl:pixel-store :unpack-alignment 1)
        (gl:tex-image-2d :texture-2d 0 :rgb image-width image-height 0 
                         (case channels
                           (3 :rgb)
                           (4 :rgba))
                         :unsigned-byte
                         (make-array (reduce #'* (array-dimensions image))
                                     :element-type '(unsigned-byte 8)
                                     :displaced-to image))
        (gl:generate-mipmap :texture-2d)
        (gl:bind-texture :texture-2d 0)))
    (draw window)))

(defmethod glut:display ((window opticl-window))
  (with-accessors ((dirty dirty)) window
    (when dirty
      (draw window)
      (setf dirty nil))))

(defmethod glut:reshape ((window opticl-window) width height)
  (setf (glut:width window) width
        (glut:height window) height)
  (with-accessors ((dirty dirty)) window
    (setf dirty t)))


#+nil
(on-initial-thread (:wait nil)
  (let ((image (opticl:make-8-bit-rgb-image 256 257)))
    (opticl:draw-rectangle image 50 50 200 200 255 0 0)
    (glut:display-window (make-opticl-window image))))

