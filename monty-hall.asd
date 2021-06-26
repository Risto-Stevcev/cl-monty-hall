(asdf:defsystem :monty-hall
  :version      "0.1.0"
  :description  "Monty hall problem simulator"
  :author       "Risto Stevcev <me@risto.codes>"
  :serial       t
  :license      "GNU GPL, version 3"
  :components   ((:file "monty-hall"))
  :depends-on   (#:alexandria #:arrow-macros))
