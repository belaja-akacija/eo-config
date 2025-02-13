(asdf/defsystem:defsystem "eo-config"
  :version "1.0.0"
  :author "Eliza Oselskyi"
  :components
  ((:file "package")
   (:file "config" :depends-on ("package"))))
