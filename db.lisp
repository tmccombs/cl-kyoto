(defpackage kyoto/db
  (:use :cl
        :kyoto/ffi
        :cffi)
  (:import-from :trivial-garbage
                #:finalize)
  (:export #:kc-db
           #:kc-open
           #:kc-close
           #:kc-reopen
           #:kc-open-p
           #:kc-error
           #:kc-handle
           #:with-kyoto-cabinet

           #:kc-clear
           #:kc-count
           #:kc-size
           #:kc-path
           #:kc-status
           #:kc-merge
           ;; error class
           #:kc-db-error
           #:kc-error-type
           #:kc-error-message))

(in-package kyoto/db)

(defclass kc-db ()
  ((handle :initarg :handle
           :reader %db-handle
           :documentation "Foreign pointer to the Kyoto Cabinet database."))
  (:documentation "An object referencing a Kyoto Cabinet."))

(define-condition kc-error (error)
  ((type :initarg :type
         :reader kc-error-type
         :type keyword)
   (msg :initarg :msg
        :reader kc-error-message
        :type string))
  (:documentation "Condition containing information about an error in Kyoto Cabinet."))

(defmethod print-object ((err kc-error) stream)
  (print-unreadable-object (err stream :type t)
    (format stream "~a ~a" (kc-error-type err) (kc-error-message err))))

(defun kc-handle (db)
  (declare (kc-db db))
  (let ((handle (%db-handle db)))
    (unless handle
      (error "Attempt to use closed database."))
    handle))

(defmethod initialize-instance :after ((db kc-db) &rest initargs &key)
  (declare (ignore initargs))
  ;; Make sure we clean up resources when garbage collected.
  (finalize db (lambda ()
                 (kc-close db))))

(defun kc-open (path &rest mode)
  "Open a Kyoto Cabinet.
PATH is a string describing the \"path\" to open, as described in the KYOTO cabinet
documentation."
  (declare (string path) (list mode))
  (let ((handle (kcdbnew)))
    (when (kcdbopen handle path mode)
      (make-instance 'kc-db :handle handle))))

(defun kc-reopen (db path &rest mode)
  "Re-Open an existing KC-DB object. You can use this to recycle the
KC-DB and its resources.
KC-REOPEN ensures the db is closed, so there is no need to close before using it."
  (declare (kc-db db) (string path))
  (let ((handle (%db-handle db)))
    (if handle
        ;; if we have a handle close it
        (kcdbclose handle)
        ;; if we don't have a handle we need to create a new one
        (setf handle (kcdbnew)
              (slot-value db 'handle) handle))
    (kcdbopen handle path mode)))

(defun kc-open-p (db)
  (declare (kc-db db))
  (not (null (%db-handle db))))

(defun kc-close (db)
  "Close a Kyoto Cabinet. Nothing can be done with it afterwards."
  (declare (kc-db db))
  (let ((handle (%db-handle db)))
    (when handle
      (kcdbclose handle)
      (kcdbdel handle)
      (setf (slot-value db 'handle) nil))))

(defmacro with-kyoto-cabinet ((var path &rest mode) &body body)
  "Execute body in a scope where VAR is bound to a Kyoto Cabinet opened
at PATH with mode(s) specified by MODE. The DB is automatically closed on completion."
  `(let ((,var (kc-open path ,@mode)))
     (unwind-protect
          (progn ,@body)
       (kc-close ,var))))

(defun kc-db-error (db)
  (declare (kc-db db))
  (let* ((handle (kc-handle db))
         (code (kcdbecode handle)))
    (unless (eq code :success)
      (make-condition 'kc-error :type code
                      :msg (kcdbemsg handle)))))

(defmacro define-simple-function (lisp-name c-name documentation)
  (declare (symbol lisp-name c-name) (string documentation))
  `(defun ,lisp-name (db)
     ,documentation
     (declare (kc-db db))
     (,c-name (kc-handle db))))

(define-simple-function kc-clear kcdbclear
  "Clear everything out of a kyoto cabinet database.")

(define-simple-function kc-count kcdbcount
  "Get the number of entries in a kyoto cabinet database.")

(define-simple-function kc-size kcdbsize
  "Get the size of the database in bytes.")

(define-simple-function kc-path kcdbpath
  "Get the path of the database as a string.")

(define-simple-function kc-status kcdbstatus
  "Get a string describing the status of the Kyoto Database")

(defun kc-merge (dest srcs &optional (mode :set))
  (declare (kc-db dest) (type (or list kc-db) srcs) (keyword mode))
  (unless (listp srcs)
    (setf srcs (list srcs)))
  (let ((len (length srcs)))
    (with-foreign-object (src-ptrs :pointer len)
      (loop
         for db in srcs
         for i from 0
         do (setf (mem-aref src-ptrs :pointer i)
                  (kc-handle db)))
      (kcdbmerge dest src-ptrs len mode))))
