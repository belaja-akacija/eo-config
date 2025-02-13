(asdf/defsystem:defsystem "eo-config"
  :author "Eliza Oselskyi"
  :components
  ((:file "package")
   (:file "config" :depends-on ("package"))))
