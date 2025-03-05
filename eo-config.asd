(asdf/defsystem:defsystem "eo-config"
  :version "1.0.3"
  :author "Eliza Oselskyi"
  :serial t
  :components
  ((:file "package")
   (:file "utils")
   (:file "testing")
   (:file "config" :depends-on ("utils"))))
