(defpackage #:weblocks-tree-widget-tests
  (:use #:cl #:weblocks #:weblocks-selenium-tests #:weblocks-selenium-tests-app #:selenium #:weblocks-tree-widget #:weblocks-utils))

(in-package :weblocks-tree-widget-tests)

(defclass test-model ()
  ((id) 
   (title :accessor test-model-title :initarg :title) 
   (content :accessor test-model-content :initarg :content)))

(defclass test-model-2 ()
  ((id) 
   (title :accessor test-model-2-title :initarg :title)
   (parent :accessor test-model-2-parent :initarg :parent)))

(defclass test-model-3 ()
  ((id) 
   (title :accessor test-model-3-title :initarg :title)
   (parent :accessor test-model-3-parent :initarg :parent)))

(defwidget just-a-tree (tree-widget)
  ())

(defwidget tree-with-advanced-template (just-a-tree)
           ())

(weblocks::deftemplate 
  :tree-branches-presentation-field-value-wt 
  'weblocks-tree-widget::tree-branches-advanced-presentation-field-value-wt 
  :context-matches (lambda (&key widget &allow-other-keys)
                     (if (typep widget 'tree-with-advanced-template)
                       10
                       0)))

(defmethod tree-data ((obj just-a-tree))
  (loop for i in (all-of 'test-model :store weblocks-selenium-tests-app::*tests-store*) 
        for m from 1 to 2
        collect 
        (list :item i 
              :children (loop for j in (all-of 'test-model :store weblocks-selenium-tests-app::*tests-store*) 
                              collect 
                              (list :item j :children 
                                    (loop for k in (all-of 'test-model :store weblocks-selenium-tests-app::*tests-store*)
                                          for l from 1 to 2 
                                          collect (list :item k :children nil)))))))

(defwidget expandable-tree (tree-widget)
  ())

(defmethod tree-data ((obj expandable-tree))
  (loop for i in (all-of 'test-model :store weblocks-selenium-tests-app::*tests-store*) 
        for m from 1 to 2
        collect 
        (list :item i 
              :children (loop for j in (find-by-values 'test-model-2 :parent i :store weblocks-selenium-tests-app::*tests-store*) 
                              collect 
                              (list :item j :children 
                                    (loop for k in (find-by-values 'test-model-3 :parent j :store weblocks-selenium-tests-app::*tests-store*)
                                          for l from 1 to 2 
                                          collect (list :item k :children nil)))))))

(defun maybe-add-demo-records-to-test-model ()
  (when (zerop (length (all-of 'test-model :store weblocks-selenium-tests-app::*tests-store*)))
    (let ((i-elem)
          (j-elem))
      (loop for i from 1 to 3  do
            (setf i-elem (persist-object 
                           weblocks-selenium-tests-app::*tests-store*
                           (make-instance 
                             'test-model
                             :title (format nil "Element - ~A" i)
                             :content "")))
            (loop for j from 1 to 3 do 
                  (setf j-elem (persist-object 
                                 weblocks-selenium-tests-app::*tests-store*
                                 (make-instance 
                                   'test-model-2
                                   :title (format nil "Element - ~A" j)
                                   :parent i-elem)))
                  (loop for k from 1 to 3 do 
                        (persist-object 
                          weblocks-selenium-tests-app::*tests-store*
                          (make-instance 
                            'test-model-3
                            :title (format nil "Element - ~A" k)
                            :parent j-elem))))))))

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
            (:h2 "Simple example")
            (:p "These are two widgets with straight-column-captions option set to t and nil")))
        (make-instance 'just-a-tree 
                       :store weblocks-selenium-tests-app::*tests-store*
                       :data-class 'test-model
                       :view (defview nil (:type tree :inherit-from '(:scaffold test-model))
                                      (title 
                                        :present-as tree-branches)
                                      (content :hidep t)))
        (make-instance 'just-a-tree 
                       :store weblocks-selenium-tests-app::*tests-store*
                       :data-class 'test-model
                       :view (defview nil (:type tree :inherit-from '(:scaffold test-model))
                                      (title 
                                        :present-as (tree-branches :straight-column-captions nil))
                                      (content :hidep t)))

        (lambda (&rest args)
          (with-html 
            (:h2 "Tree with advanced template")
            (:p "Similar example just with other template")))

        (make-instance 'tree-with-advanced-template 
                       :store weblocks-selenium-tests-app::*tests-store*
                       :data-class 'test-model
                       :view (defview nil (:type tree :inherit-from '(:scaffold test-model))
                                      (title 
                                        :present-as tree-branches)
                                      (content :hidep t)))

        (make-instance 'tree-with-advanced-template 
                       :store weblocks-selenium-tests-app::*tests-store*
                       :data-class 'test-model
                       :view (defview nil (:type tree :inherit-from '(:scaffold test-model))
                                      (title 
                                        :present-as (tree-branches :straight-column-captions nil))
                                      (content :hidep t)))

        (lambda (&rest args)
          (with-html 
            (:h2 "Tree with collapse/expand functionality")
            (:p "Click on the actions to see it in action")))

        (let ((tree))
          (flet ((expand-or-collapse-allowed-p (&key item &allow-other-keys)
                   (not (typep item 'test-model-3))))
            (setf tree
                  (make-instance 'expandable-tree 
                    :store weblocks-selenium-tests-app::*tests-store*
                    :data-class 'test-model
                    :expand-all-items-p nil
                    :view (defview nil (:type tree :inherit-from '(:scaffold test-model))
                                   (title 
                                     :present-as tree-branches)
                                   (content :hidep t)
                                   (actions :present-as html 
                                            :reader (action-links-reader 
                                                      (lambda () tree)
                                                      nil :adding-allowed-p nil 
                                                      :editing-allowed-p nil
                                                      :deleting-allowed-p nil 
                                                      :expand-allowed-p #'expand-or-collapse-allowed-p 
                                                      :collapse-allowed-p #'expand-or-collapse-allowed-p)))))))

        (lambda (&rest args)
          (with-html 
            (:div :style "clear:both")
            (:style :type "text/css"
             (str 
               (css-lite:css 
                 (("#root .function")
                  (:clear "both"))
                 ((".tree-widget")
                  (:float "left"
                   :padding-right "10px"))))))
          (render-link (lambda (&rest args)
                         (answer widget t)) "back"))))
    (do-page widget)))

(define-demo-action "Tree widget demos" #'tree-widget-demonstration-action :prototype-engine-p t)
