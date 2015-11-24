
(uiop:define-package kyoto-cabinet
  (:nicknames :kyoto :kc)
  (:use :cl)
  (:use-reexport :kyoto/db
                 :kyoto/accessors
                 :kyoto/cursor
                 :kyoto/transaction))

(in-package kyoto-cabinet)
