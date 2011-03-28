
(asdf:defsystem :cl-opengl-test
  :name "cl-opengl-test"
  :author "Cyrus Harmon <ch-lisp@bobobeach.com>"
  :licence "BSD"
  :default-component-class cl-source-file
  :depends-on (opticl)
  :serial t
  :components
  ((:file "package")
   (:file "cl-opengl-test")
   (:file "opticl-opengl")))
