
(in-package :cl-opengl-test)

(defclass opticl-window (glut:window)
  ((texture :accessor texture)
   (framebuffer :accessor framebuffer)
   (image :accessor image :initarg :image))
  (:default-initargs :width 512 :height 512
                     :title "Opticl Image Viewer"
                     :x 100 :y 100 :mode '(:double :rgb :depth)
                     :image (opticl:read-image-file
                             "/Users/sly/projects/opticl-test/images/truck.jpeg")))

(defmethod glut:display-window :before ((win opticl-window))
  (let ((framebuffer (first (gl:gen-framebuffers-ext 1)))
        (depthbuffer (first (gl:gen-renderbuffers-ext 1)))
        (texture (first (gl:gen-textures 1))))
    ;; setup framebuffer
    (gl:bind-framebuffer-ext :framebuffer-ext framebuffer)
    
    ;; setup texture and attach it to the framebuffer
    (gl:bind-texture :texture-2d texture)
    (gl:tex-parameter :texture-2d :texture-min-filter :linear-mipmap-linear)
    (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
    
    (with-accessors ((image image)) win
      (opticl:with-image-bounds (height width) image
        (gl:tex-image-2d :texture-2d 0 :rgb width height 0 :rgb :unsigned-byte
                         (make-array (reduce #'* (array-dimensions image))
                                     :element-type '(unsigned-byte 8) :displaced-to image))

        (gl:generate-mipmap-ext :texture-2d)
        (gl:bind-texture :texture-2d 0)
        (gl:framebuffer-texture-2d-ext :framebuffer-ext
                                       :color-attachment0-ext
                                       :texture-2d
                                       texture
                                       0)
        
        ;; setup depth-buffer and attach it to the framebuffer
        (gl:bind-renderbuffer-ext :renderbuffer-ext depthbuffer)
        (gl:renderbuffer-storage-ext :renderbuffer-ext :depth-component24 width height)
        (gl:framebuffer-renderbuffer-ext :framebuffer-ext
                                         :depth-attachment-ext
                                         :renderbuffer-ext
                                         depthbuffer)))
    
    ;; validate framebuffer
    (let ((framebuffer-status (gl:check-framebuffer-status-ext :framebuffer-ext)))
      (unless (gl::enum= framebuffer-status :framebuffer-complete-ext)
        (error "Framebuffer not complete: ~A." framebuffer-status)))

    (setf (texture win) texture
          (framebuffer win) framebuffer)))

(defmethod glut:display ((win opticl-window))

  (gl:load-identity)
  (gl:bind-framebuffer-ext :framebuffer-ext 0)

  (gl:viewport 0 0 (glut:width win) (glut:height win))
  (gl:clear-color 0 0 0 0)       ; background will be black
  (gl:clear-depth 1)              ; clear buffer to maximum depth
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  

  (with-accessors ((image image)) win
    (opticl:with-image-bounds (height width) image
    
      (gl:viewport (/ (- (glut:width win) width) 2)
                   (/ (- (glut:height win) height) 2)
                   width height)

      (let ((texture (texture win)))  
        (gl:enable :texture-2d)
        (gl:bind-texture :texture-2d texture)
        (gl:push-matrix)

        (gl:with-primitives :quads
          (gl:tex-coord 0 0) (gl:vertex -1 1)
          (gl:tex-coord 1 0) (gl:vertex 1 1)
          (gl:tex-coord 1 1) (gl:vertex 1 -1)
          (gl:tex-coord 0 1) (gl:vertex -1 -1))
        (gl:pop-matrix)))
    (glut:swap-buffers)))

(defmethod glut:idle ((window opticl-window))
  (glut:post-redisplay))

(defmethod glut:reshape ((window opticl-window) width height)
  (setf (glut:width window) width
        (glut:height window) height))
