(defpackage kyoto/transaction
  (:use :cl
        :kyoto/ffi
        :kyoto/db)
  (:export #:kc-start-transaction
           #:kc-end-transaction
           #:with-kc-transaction))

(in-package kyoto/transaction)

(defun kc-start-transaction (db &key hard try)
  "Start a transaction on a Kyoto Cabinet. If HARD is true,
use physical synchronization with the device, otherwise use logical
synchronization. If try is non-nil then only try to acquire a transaction."
  (declare (kc-db db))
  (if try
      (kcdbbegintrantry (kc-handle db) hard)
      (kcdbbegintran (kc-handle db) hard)))

(defun kc-end-transaction (db &optional (commit t))
  "End a transaction. If COMMIT is true (default)
commit the transaction, otherwise abort it."
  (kcdbendtran (kc-handle db) commit))

(defmacro with-kc-transaction ((db) &body body)
  (let ((db-var (gensym)))
    `(let ((,db-var ,db))
       (when (kc-start-transaction ,db-var)
         (unwind-protect
              (progn ,@body)
           (kc-end-transaction ,db-var))))))
