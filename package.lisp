(defpackage :eo-config
  (:use :cl)
  ;; CONFIG
  (:export
    #:define-allowed-names
    #:load-config
    #:make-config-globals
    #:set-rules
    #:test-rule
    ; special globals
    #:*config-allowed-names*
    #:*config-parameter-rules*
    ))
