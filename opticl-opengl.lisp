
(in-package :cl-opengl-test)

(defclass opticl-window (glut:window)
  ((texture :accessor texture)
   (framebuffer :accessor framebuffer))
  (:default-initargs :width 512 :height 512
                     :title "Opticl Image Viewer"
                     :x 100 :y 100 :mode '(:double :rgb :depth)))

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

    (let ((img 
           (opticl:fit-image-into (opticl:read-image-file
                                   "/Users/sly/projects/opticl-test/images/truck.jpeg")
                                  :y-max 512 :x-max 512 :pad t) 
            #+nil (opticl:make-8-bit-rgb-image 512 512 :initial-element 128)))
      (gl:tex-image-2d :texture-2d 0 :rgb 512 512 0 :rgb :unsigned-byte
                       (make-array (reduce #'* (array-dimensions img))
                                   :element-type '(unsigned-byte 8) :displaced-to img)))

    (gl:generate-mipmap-ext :texture-2d)
    (gl:bind-texture :texture-2d 0)
    (gl:framebuffer-texture-2d-ext :framebuffer-ext
                                   :color-attachment0-ext
                                   :texture-2d
                                   texture
                                   0)

    ;; setup depth-buffer and attach it to the framebuffer
    (gl:bind-renderbuffer-ext :renderbuffer-ext depthbuffer)
    (gl:renderbuffer-storage-ext :renderbuffer-ext :depth-component24 512 512)
    (gl:framebuffer-renderbuffer-ext :framebuffer-ext
                                     :depth-attachment-ext
                                     :renderbuffer-ext
                                     depthbuffer)
    
    ;; validate framebuffer
    (let ((framebuffer-status (gl:check-framebuffer-status-ext :framebuffer-ext)))
      (unless (gl::enum= framebuffer-status :framebuffer-complete-ext)
        (error "Framebuffer not complete: ~A." framebuffer-status)))

    (setf (texture win) texture
          (framebuffer win) framebuffer)))

(defmethod glut:display ((win opticl-window))

  (gl:load-identity)
  (gl:bind-framebuffer-ext :framebuffer-ext 0)

  (gl:viewport (/ (- (glut:width win) 512) 2)
               (/ (- (glut:height win) 512) 2)
               512 512)
  

  (let ((texture (texture win)))  
    (gl:enable :texture-2d)
    (gl:bind-texture :texture-2d texture)
    (gl:push-matrix)

    (gl:with-primitives :quads
      (gl:tex-coord 0 0) (gl:vertex -1 1)
      (gl:tex-coord 1 0) (gl:vertex 1 1)
      (gl:tex-coord 1 1) (gl:vertex 1 -1)
      (gl:tex-coord 0 1) (gl:vertex -1 -1))
    (gl:pop-matrix))
  (glut:swap-buffers))

(defmethod glut:idle ((window opticl-window))
  (glut:post-redisplay))

(defmethod glut:reshape ((window opticl-window) width height)
  (setf (glut:width window) width
        (glut:height window) height))
