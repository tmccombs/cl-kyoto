
(defpackage cl-kyoto-asdf
  (:use :cl :asdf))

(in-package :cl-kyoto-asdf)

(eval-when (:load-toplevel :execute)
  (operate 'load-op :cffi-grovel))

(defsystem cl-kyoto
  :description "Lisp bindings for Kyoto Cabinet"
  :author "Thayne McCombs"
  :version "0.1.0"
  :defsystem-depends-on (:cffi-grovel)
  :depends-on (:cffi
               :trivial-garbage
               :uiop)
  :components ((:file "grovel-package")
               (cffi-grovel:grovel-file "kyoto-grovel" :depends-on ("grovel-package"))
               (:file "kyoto-ffi" :depends-on ("kyoto-grovel"))
               (:file "helpers" :depends-on ("kyoto-ffi"))
               (:file "db" :depends-on ("kyoto-ffi"))
               (:file "accessors" :depends-on ("db" "helpers" "kyoto-ffi"))
               (:file "cursor" :depends-on ("db" "helpers" "kyoto-ffi"))
               (:file "transaction" :depends-on ("db" "kyoto-ffi"))
               (:file "kyoto" :depends-on ("db" "accessors" "cursor" "transaction"))))
