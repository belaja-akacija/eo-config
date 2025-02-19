(asdf/defsystem:defsystem "eo-config"
  :version "1.0.2"
  :author "Eliza Oselskyi"
  :serial t
  :components
  ((:file "package")
   (:file "utils" :depends-on ("package"))
   (:file "config" :depends-on ("package" "utils"))))
