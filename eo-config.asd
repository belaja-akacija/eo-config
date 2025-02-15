(asdf/defsystem:defsystem "eo-config"
  :version "1.0.1"
  :author "Eliza Oselskyi"
  :components
  ((:file "package")
   (:file "utils" :depends-on ("package"))
   (:file "config" :depends-on ("package" "utils"))))
