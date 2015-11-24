(defpackage kyoto/accessors
  (:use :cl
        :kyoto/ffi
        :kyoto/helpers
        :kyoto/db
        :cffi)
  (:export #:kc-set
           #:kc-add
           #:kc-replace
           #:kc-append
           #:kc-get
           #:kc-seize
           #:kc-cas
           #:kc-check))

(in-package kyoto/accessors)

(defmacro define-setter (lisp-name cname documentation)
  (declare (symbol lisp-name cname) (string documentation))
  `(defun ,lisp-name (db key val)
     ,documentation
     (declare (kc-db db) (buffer-in-spec key val))
     (with-buffer (kbuf ksize key)
       (with-buffer (vbuf vsize val)
         (,cname (kc-handle db) kbuf ksize vbuf vsize)))))

(define-setter kc-set kcdbset "Set a value in a Kyoto Cabinet")
(define-setter kc-add kcdbadd "Add a value to a Kyoto Cabinet")
(define-setter kc-replace kcdbreplace "Replace a value")
(define-setter kc-append kcdbappend "Append to the value.")


(defmacro define-getter (lisp-name cname documentation)
  (declare (symbol lisp-name cname) (string documentation))
  `(defun ,lisp-name (db key &optional (type :string))
     ,documentation
     (declare (kc-db db) (buffer-out-spec type) (buffer-in-spec key))
     (with-buffer (kbuf ksize key)
       (with-foreign-object (vsize 'size_t)
         (with-result (vbuf (,cname (kc-handle db) kbuf ksize vsize))
           (buffer-to-lisp vbuf (mem-ref vsize 'size_t) type))))))

(define-getter kc-get kcdbget "Get the value stored in the cabinet, or nil if not found.
TYPE can be :string or :octets and specifies the return type of the function.")
(define-getter kc-seize kcdbseize "Get the value stored in the cabinet for a key and remove it if found.
Return nil if not found.
TYPE can be :string or :octets and specifies the return type of the function.")

(defun kc-cas (db key old-value new-value)
  "Compare and swap operation."
  (with-buffer (kbuf ksize key)
    (with-buffer (ovbuf ovsize old-value)
      (with-buffer (nvbuf nvsize new-value)
        (kcdbcas (kc-handle db) kbuf ksize ovbuf ovsize nvbuf nvsize)))))

(defun kc-check (db key)
  "Test if the database contains a value.
If successful returns the number of bytes in the value, otherwise returns nil."
  (with-buffer (kbuf ksize key)
    (let ((n (kcdbcheck (kc-handle db) kbuf ksize)))
      (unless (minusp n) n))))
