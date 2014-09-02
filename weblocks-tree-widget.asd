;;;; weblocks-tree-widget.asd

(asdf:defsystem #:weblocks-tree-widget
  :serial t
  :description "A tree widget for weblocks"
  :author "Olexiy Zamkoviy <olexiy.z@gmail.com>"
  :license "LLGPL"
  :version "0.3.5"
  :depends-on (#:weblocks #:yaclml #:alexandria)
  :components ((:file "package")
               (:file "weblocks-tree-widget")))

