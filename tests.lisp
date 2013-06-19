(defpackage #:weblocks-tree-widget-tests
  (:use #:cl #:weblocks #:weblocks-selenium-tests #:weblocks-selenium-tests-app #:selenium #:weblocks-tree-widget #:weblocks-utils))

(in-package :weblocks-tree-widget-tests)

(defclass test-model ()
  ((id) 
   (title :accessor test-model-title :initarg :title) 
   (content :accessor test-model-content :initarg :content)))

(defmethod tree-data ((obj tree-widget))
  (loop for i in (all-of 'test-model) 
        for m from 1 to 2
        collect 
        (list :item i 
              :children (loop for j in (all-of 'test-model) 
                              collect 
                              (list :item j :children 
                                    (loop for k in (all-of 'test-model)
                                          for l from 1 to 2 
                                          collect (list :item k :children nil)))))))

(defun maybe-add-demo-records-to-test-model ()
  (when (zerop (length (all-of 'test-model)))
    (loop for i from 1 to 3  do
          (persist-object 
            weblocks-stores:*default-store*
            (make-instance 
              'test-model
              :title (format nil "Element - ~A" i)
              :content "")))))

(defun tree-widget-demonstration-action (&rest args)
  (maybe-add-demo-records-to-test-model)
  (let ((widget (make-instance 'composite)))
    (setf 
      (composite-widgets widget) 
      (list 
        (lambda (&rest args)
          (with-html 
            (:h1 "Tree widget demo")
            (:hr)
            (:p "These are two widgets with straight-column-captions option set to t and nil")))
        (make-instance 'tree-widget 
                       :view (defview nil (:type tree :inherit-from '(:scaffold test-model))
                                      (title 
                                        :present-as tree-branches))
                       :data-class 'webapp-cls)
        (make-instance 'tree-widget 
                       :view (defview nil (:type tree :inherit-from '(:scaffold test-model))
                                      (title 
                                        :present-as (tree-branches :straight-column-captions nil)))
                       :data-class 'webapp-cls)
        (lambda (&rest args)
          (with-html 
            (:style :type "text/css"
             ".tree-widget { float:left;margin-right:10px; }"))
          (render-link (lambda (&rest args)
                         (answer widget t)) "back"))))
    (do-page widget)))

(define-demo-action "Tree widget demos" #'tree-widget-demonstration-action :prototype-engine-p t)
