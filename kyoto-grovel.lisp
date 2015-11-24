(in-package kyoto/grovel)

(pkg-config-cflags "kyotocabinet" :optional t)

(include "kclangc.h")

(ctype size_t "size_t")

(cenum error-code
       ((:success "KCESUCCESS") :documentation "Success")
       ((:not-implemented "KCENOIMPL") :documentation "Not implemented")
       ((:invalid "KCEINVALID") :documentation "invalid operatio")
       ((:no-repository "KCENOREPOS") :documentation "no repository")
       ((:no-permission "KCENOPERM") :documentation "no permission")
       ((:broken "KCEBROKEN") :documentation "broken file")
       ((:duplicate "KCEDUPREC") :documentation "record duplication")
       ((:no-record "KCENOREC") :documentation "no record")
       ((:inconsistent "KCELOGIC") :documentation "logical inconsistency")
       ((:system-error "KCESYSTEM") :documentation "system error")
       ((:misc "KCEMISC") :documentation "miscellaneous error"))

#| ;; for some future version of cffi
(bitfield-enum open-flags
       ((:reader "KCOREADER") :documentation "Open as reader")
       ((:writer "KCOWRITER") :documentation "Open as writer")
       ((:create "KCOCREATE") :documentation "Writer creating")
       ((:truncate "KCOTRUNCATE") :documentation "writer truncating")
       ((:autotransaction "KCOAUTOTRAN") :documentation "auto transaction")
       ((:autosync "KCOAUTOSYNC") :documentation "auto synchronization")
       ((:nolock "KCONOLOCK") :documentation "open without locking")
       ((:trylock "KCOTRYLOCK") :documentation "lock without blocking")
       ((:norepair "KCONOREPAIR") :documentation "open without auto repair"))
|#
(bitfield open-flags
          ((:reader) :optional t)
          ((:writer) :optional t)
          ((:create) :optional t)
          ((:truncate) :optional t)
          ((:autotransaction) :optional t)
          ((:autosync) :optional t)
          ((:nolock) :optional t)
          ((:trylock) :optional t)
          ((:norepair) :optional t))

(cenum merge-mode
       ((:set "KCMSET") :documentation "overwrite the existing value")
       ((:add "KCMADD") :documentation "keep the existing value")
       ((:replace "KCMREPLACE") :documentation "modify the existing record only")
       ((:append "KCMAPPEND") :documentation "append the new value"))

(cvar ("KCVERSION" +kc-version+ :read-only t) :string)
