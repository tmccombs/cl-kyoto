(defpackage kyoto/cursor
  (:use :cl
        :kyoto/ffi
        :kyoto/helpers
        :kyoto/db
        :cffi)
  (:import-from :trivial-garbage
                #:finalize)
  (:export #:kc-cursor
           #:kc-db-cursor
           #:with-kc-cursor
           #:kc-cursor-set
           #:kc-cursor-remove
           #:kc-cursor-get-key
           #:kc-cursor-get-value
           #:kc-cursor-get
           #:kc-cursor-seize
           #:kc-cursor-jump-start
           #:kc-cursor-jump-to
           #:kc-cursor-jump-end
           #:kc-cursor-jump-back-to
           #:kc-cursor-forward
           #:kc-cursor-backward))

(in-package kyoto/cursor)

(defclass kc-cursor ()
  ((cur :initarg :cur
        :reader %cursor))
  (:documentation "A cursor over a Kyoto Cabinet."))

(defun kc-db-cursor (db)
  "Get a cursor for a Kyoto Cabinet database."
  (declare (kc-db db))
  (let* ((cur (kcdbcursor (kc-handle db)))
         (ret (make-instance 'kc-cursor :cur cur)))
    (finalize ret (lambda ()
                    (kccurdel cur)))))

(defmacro with-kc-cursor ((cur db) &body body)
  (let ((handle (gensym)))
    `(let* ((,handle (kcdbcursor (kc-handle ,db)))
            (,cur (make-instance 'kc-cursor :cur ,handle)))
       (unwind-protect
            (progn ,@body)
         (kccurdel ,handle)))))

(defun kc-cursor-set (cur val &optional step)
  "Set the value at the current location of a cursor"
  (with-buffer (vbuf vsize val)
    (kccursetvalue (%cursor cur) vbuf vsize step)))

(defun kc-cursor-remove (cur)
  "Remove the value at the current cursor location from the database."
  (declare (kc-cursor cur))
  (kccurremove (%cursor cur)))

(defun kc-cursor-get-key (cur &optional (type :string) step)
  "Get the key at the current position of the cursor. If STEP is true, move
to the next position as well. Type is the return type, and can be :string or :octets"
  (declare (kc-cursor cur) (buffer-out-spec type))
  (with-foreign-object (ksize 'size_t)
    (with-result (kbuf (kccurgetkey (%cursor cur) ksize step))
      (buffer-to-lisp kbuf (mem-ref ksize 'size_t) type))))

(defun kc-cursor-get-value (cur &optional (type :string) step)
  "Get the value at the current position of the cursor."
  (declare (kc-cursor cur) (buffer-out-spec type))
  (with-foreign-object (vsize 'size_t)
    (with-result (vbuf (kccurgetvalue (%cursor cur) vsize step))
      (buffer-to-lisp vbuf (mem-ref vsize 'size_t) type))))

(defun kc-cursor-get (cur &optional (type :string) step)
  "Get the key and value at the current location of the cursor. The type of the
returned key and value are both specified by TYPE, which can be :string or :octets.
If step is true the cursor steps forward."
  (declare (kc-cursor cur) (buffer-out-spec type))
  (with-foreign-objects ((ksize 'size_t)
                         (vsize 'size_t)
                         (vbuf-pointer :pointer))
    (with-result (kbuf (kccurget (%cursor cur) ksize
                                 vbuf-pointer vsize
                                 step))
      (values (buffer-to-lisp kbuf
                              (mem-ref ksize 'size_t) type)
              (buffer-to-lisp (mem-ref vbuf-pointer :pointer)
                              (mem-ref vsize 'size_t) type)))))

(defun kc-cursor-seize (cur &optional (type :string))
  "Same as KC-CURSOR-GET, but it also removes the pair from the database.
The cursor is implicitly moved to the next record."
  (declare (kc-cursor cur) (buffer-out-spec type))
  (with-foreign-objects ((ksize 'size_t)
                         (vsize 'size_t)
                         (vbuf-pointer :pointer))
    (with-result (kbuf (kccurseize (%cursor cur) ksize
                                 vbuf-pointer vsize))
      (values (buffer-to-lisp kbuf (mem-ref ksize 'size_t) type)
              (buffer-to-lisp (mem-ref vbuf-pointer :pointer)
                              (mem-ref vsize 'size_t) type)))))

(defun kc-cursor-jump-start (cur)
  "Jump the cursor to the beginning of the database."
  (declare (kc-cursor cur))
  (kccurjump (%cursor cur)))

(defun kc-cursor-jump-to (cur key)
  "Jump the cursor to the position of KEY.
Returns true if successful, false otherwise."
  (declare (kc-cursor cur))
  (with-buffer (kbuf ksize key)
    (kccurjumpkey (%cursor cur) kbuf ksize)))

(defun kc-cursor-jump-end (cur)
  "Jump the cursor to the end of the database."
  (declare (kc-cursor cur))
  (kccurjumpback (%cursor cur)))

(defun kc-cursor-jump-back-to (cur key)
  "Jump backwards to the position of KEY.
Returns true if successful, false otherwise."
  (declare (kc-cursor cur))
  (with-buffer (kbuf ksize key)
    (kccurjumpbackkey (%cursor cur) kbuf ksize)))

(defun kc-cursor-forward (cur)
  "Step the cursor forward one record."
  (declare (kc-cursor cur))
  (kccurstep (%cursor cur)))

(defun kc-cursor-backward (cur)
  "Step the cursor backward one record."
  (declare (kc-cursor cur))
  (kccurstepback (%cursor cur)))
