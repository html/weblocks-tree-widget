;;;; weblocks-tree-widget-tests.asd

(asdf:defsystem #:weblocks-tree-widget-tests
 :serial t
 :description "Tests for weblocks-tree-widget"
 :author "Olexiy Zamkoviy <olexiy.z@gmail.com>"
 :license "LLGPL"
 :version "0.0.2"
 :depends-on (#:weblocks-tree-widget #:weblocks-selenium-tests #:weblocks-utils #:css-lite)
 :components 
 ((:file "tests")))

