(defpackage kyoto/helpers
  (:use :cl
        :cffi
        :kyoto/ffi)
  (:export #:with-result
           #:buffer-to-lisp
           #:with-buffer

           #:buffer-out-spec
           #:buffer-in-spec))

(in-package kyoto/helpers)

(deftype buffer-out-spec ()
  "A specification for a return type from
buffer-to-lisp."
  '(member :string :octets))
(deftype buffer-in-spec ()
  "Specification for a buffer used with with-buffer"
  '(or string (vector (unsigned-byte 8))))

(defmacro with-result ((ptr initform) &body body)
  "Bind PTR to the result of INITFORM, which should return a buffer
allocated by kyoto cabinet. The memory is freed after body has executed.
Body is only executed if PTR is non-nil and not the null pointer. Otherwise nil
is returned."
  `(let ((,ptr ,initform))
     (when (and ,ptr (not (null-pointer-p ,ptr)))
       (unwind-protect
            (progn ,@body)
         (kcfree ,ptr)))))

(defun buffer-to-lisp (buf size type)
  "Convert a foreign buffer to a lisp type specified by TYPE."
  (declare (integer size) (buffer-out-spec type))
  (ecase type
    (:string
     (foreign-string-to-lisp buf :count size))
    (:octets
     (foreign-to-octets buf size))))

(defun foreign-to-octets (ptr size)
  "Convert a pointer and size to a lisp vector of octets."
  (let ((result (make-array size :element-type '(unsigned-byte 8))))
    (dotimes (i size result)
      (setf (aref result i) (mem-ref ptr :unsigned-char i)))))

(defmacro with-buffer ((buf size val) &body body)
  `(etypecase ,val
     (string
      (with-foreign-string ((,buf ,size) ,val :null-terminated-p nil)
        ,@body))
     ((vector (unsigned-byte 8))
      (with-pointer-to-vector-data (,buf ,val)
        (let ((,size (length ,val)))
          ,@body)))))
