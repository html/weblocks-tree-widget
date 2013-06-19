Weblocks tree widget, a tree based on weblocks gridedit widget.

To use it override tree-data method, see method documentation.
Sample code is 

(make-instance 'tree-widget 
   :view (defview nil (:type tree)
     (data 
      :present-as tree-branches))
   :data-class 'webapp-cls)
