(defpackage kyoto/ffi
  (:use :cl
        :cffi
        :kyoto/grovel)
  (:export #:kcdbnew
           #:kcdbdel
           #:kcdbecode
           #:kcdbemsg
           #:kcdbopen
           #:kcdbclose
           #:kcdbclear
           #:kcdbcount
           #:kcdbsize
           #:kcdbpath
           #:kcdbstatus
           #:kcdbdumpsnap
           #:kcdbloadsnap
           #:kcdbmerge
           ;;accessors
           #:kcdbset
           #:kcdbadd
           #:kcdbreplace
           #:kcdbappend
           #:kcdbincrint
           #:kcdbinrdouble
           #:kcdbcas
           #:kcdbremove
           #:kcdbget
           #:kcdbget
           #:kcdbseize
           #:kcdbcheck
           ;;cursor
           #:kcdbcursor
           #:kccurdel
           #:kccursetvalue
           #:kccurremove
           #:kccurgetkey
           #:kccurgetvalue
           #:kccurget
           #:kccurseize
           #:kccurjump
           #:kccurjumpkey
           #:kccurjumpback
           #:kccurjumpbackkey
           #:kccurstep
           #:kccurstepback
           ;; transactions
           #:kcdbbegintran
           #:kcdbbegintrantry
           #:kcdbendtran

           #:kcfree

           #:size_t))

(in-package kyoto/ffi)

(define-foreign-library libkc
  (t (:default "libkyotocabinet")))
(use-foreign-library libkc)

(defctype kc-bool (:boolean :int32))

(defcfun "kcfree" :void
  (ptr :pointer))

;; general database operations
(defcfun "kcdbnew" :pointer)
(defcfun "kcdbdel" :void
  (db :pointer))

(defcfun "kcdbecode" error-code
  (db :pointer))

(defcfun "kcdbemsg" :string
  (db :pointer))

(defcfun "kcdbopen" kc-bool
  (db :pointer)
  (path :string)
  (mode open-flags))

(defcfun "kcdbclose" kc-bool
  (db :pointer))

(defcfun "kcdbclear" kc-bool
  (db :pointer))
(defcfun "kcdbcount" :int64
  (db :pointer))
(defcfun "kcdbsize" :int64
  (db :pointer))
(defcfun "kcdbpath" :string
  (db :pointer))
(defcfun "kcdbstatus" :string
  (db :pointer))

(defcfun "kcdbdumpsnap" kc-bool
  (db :pointer)
  (dest :string))
(defcfun "kcdbloadsnap" kc-bool
  (db :pointer)
  (src :string))

(defcfun "kcdbmerge" kc-bool
  (db :pointer)
  (srcary (:pointer :pointer))
  (srcnum size_t)
  (mode merge-mode))


;; database getters and setters
(defcfun "kcdbset" kc-bool
  (db :pointer)
  (kbuf :pointer)
  (ksiz size_t)
  (vbuf :pointer)
  (vsiz size_t))

(defcfun "kcdbadd" kc-bool
  (db :pointer )
  (kbuf :pointer)
  (ksiz size_t)
  (vbuf :pointer)
  (vsiz size_t))

(defcfun "kcdbreplace" kc-bool
  (db :pointer)
  (kbuf :pointer)
  (ksiz size_t)
  (vbuf :pointer)
  (vsiz size_t))

(defcfun "kcdbappend" kc-bool
  (db :pointer)
  (kbuf :pointer)
  (ksiz size_t)
  (vbuf :pointer)
  (vsiz size_t))

(defcfun "kcdbincrint" :int64
  (db :pointer)
  (kbuf :pointer)
  (ksiz size_t)
  (num :int64)
  (orig :int64))

(defcfun "kcdbincrdouble" :double
  (db :pointer)
  (kbuf :pointer)
  (ksiz size_t)
  (num :double)
  (orig :double))

(defcfun "kcdbcas" kc-bool
  (db :pointer)
  (kbuf :pointer)
  (ksiz size_t)
  (ovbuf :pointer)
  (ovsiz size_t)
  (nvbuf :pointer)
  (nvsiz size_t))

(defcfun "kcdbremove" kc-bool
  (db :pointer)
  (kbuf :pointer)
  (ksiz size_t))

(defcfun "kcdbget" :pointer
  (db :pointer)
  (kbuf :pointer)
  (ksiz size_t)
  (sp (:pointer size_t)))

(defcfun "kcdbcheck" :int32
  (db :pointer)
  (kbuf :pointer)
  (ksiz size_t))

(defcfun "kcdbseize" :pointer
  (db :pointer)
  (kbuf :pointer)
  (ksiz size_t)
  (sp (:pointer size_t)))

;; Cursor

(defcfun "kcdbcursor" :pointer
  (db :pointer))
(defcfun "kccurdel" :void
  (cur :pointer))

(defcfun "kccursetvalue" kc-bool
  (cur :pointer)
  (vbuf :pointer)
  (vsiz size_t)
  (step kc-bool))
(defcfun "kccurremove" kc-bool
  (cur :pointer))
(defcfun "kccurgetkey" :pointer
  (cur :pointer)
  (sp (:pointer size_t))
  (step kc-bool))
(defcfun "kccurgetvalue" :pointer
  (cur :pointer)
  (sp (:pointer size_t))
  (step kc-bool))
(defcfun "kccurget" :pointer
  (cur :pointer)
  (ksp (:pointer size_t))
  (vbp (:pointer :pointer))
  (vsp (:pointer size_t))
  (step kc-bool))
(defcfun "kccurseize" :pointer
  (cur :pointer)
  (ksp (:pointer size_t))
  (vbp (:pointer :pointer))
  (vsp (:pointer size_t)))

(defcfun "kccurjump" kc-bool
  (cur :pointer))
(defcfun "kccurjumpkey" kc-bool
  (cur :pointer)
  (kbuf :pointer)
  (ksiz size_t))
(defcfun "kccurjumpback" kc-bool
  (cur :pointer))
(defcfun "kccurjumpbackkey" kc-bool
  (cur :pointer)
  (kbuf :pointer)
  (ksiz size_t))
(defcfun "kccurstep" kc-bool
  (cur :pointer))
(defcfun "kccurstepback" kc-bool
  (cur :pointer))


;; transactions
(defcfun "kcdbbegintran" kc-bool
  (db :pointer)
  (hard kc-bool))
(defcfun "kcdbbegintrantry" kc-bool
  (db :pointer)
  (hard kc-bool))
(defcfun "kcdbendtran" kc-bool
  (db :pointer)
  (commit kc-bool))
