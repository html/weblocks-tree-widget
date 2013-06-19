;;;; weblocks-tree-widget.asd

(asdf:defsystem #:weblocks-tree-widget
  :serial t
  :description "A tree widget for weblocks"
  :author "Olexiy Zamkoviy <olexiy.z@gmail.com>"
  :license "LLGPL"
  :version "0.1.0"
  :depends-on (#:weblocks)
  :components ((:file "package")
               (:file "weblocks-tree-widget")))

