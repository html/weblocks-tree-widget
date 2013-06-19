;;;; weblocks-tree-widget.lisp

(in-package #:weblocks-tree-widget)

(defwidget tree-widget (gridedit)
           ()
           (:default-initargs 
             :allow-pagination-p nil 
             :allow-add-p nil 
             :allow-delete-p nil 
             :allow-select-p nil))

(defmethod dataseq-render-mining-bar ((obj tree-widget) &rest args)
  (declare (ignore args)))

(defmethod render-dataseq-body ((obj tree-widget) &rest args)
  (let ((data-sequence (tree-data obj)))
    (setf (slot-value obj 'weblocks::rendered-data-sequence) nil)
    (write-string 
      (weblocks::dataseq-body-wt 
        :content
        (weblocks::capture-weblocks-output 
          (apply #'render-object-view data-sequence (dataseq-view obj)
                 :widget obj
                 :summary (if (dataseq-sort obj)
                            (format nil "Ordered by ~A, ~A."
                                    (string-downcase
                                      (humanize-name
                                        (dataseq-sort-slot obj)))
                                    (string-downcase
                                      (humanize-sort-direction
                                        (dataseq-sort-direction obj))))
                            nil)
                 :custom-fields nil #+l(weblocks::append-custom-fields
                                  (remove nil
                                          (list
                                            (when (dataseq-allow-select-p obj)
                                              (cons 0 (weblocks::make-select-field obj)))
                                            (when (and (dataseq-allow-drilldown-p obj)
                                                       (dataseq-on-drilldown obj))
                                              (weblocks::make-drilldown-field obj))))
                                  args)
                 args))) 
      *weblocks-output-stream*)

    (setf (slot-value obj 'weblocks::rendered-data-sequence) data-sequence)))

(defmethod tree-data ((obj tree-widget))
  "Should return (list (list :item #<store-object> :children (list (list :item #<store-object>) ...)) ...)"
  (error "Implement tree-data method for class ~A~%" (class-of obj)))

(defclass tree-view (table-view) 
  ())

(defclass tree-scaffold (table-scaffold)
  ())

(defclass tree-view-field (table-view-field)
  ((current-row-info :accessor tree-view-field-current-row-info)))

(defun tree-view-body-row-wt (&key prefix suffix row-class content children-content)
  (yaclml:with-yaclml-output-to-string
    (<:as-is prefix)
    (<:tr :class row-class
          (<:as-is content))
    (<:as-is children-content)
    (<:as-is suffix)))

(weblocks::deftemplate :tree-view-body-row-wt 'tree-view-body-row-wt)

;; Table body
(defmethod with-table-view-body-row ((view tree-view) obj widget &rest args &key alternp (level 0) lastp single-child-p lastp-map levels-count &allow-other-keys)
  (let ((tree-branches-data 
          (list 
            :single-child-p single-child-p
            :lastp-map  
            (loop for i from 1 to level 
                  for j in lastp-map 
                  collect j)
            :level level
            :levels-left (loop for i from 1 to (- levels-count level 1) 
                               collect (list 
                                         :firstp (= i 1)
                                         :num i))
            :lastp lastp)))
    (map-view-fields 
      (lambda (field-info &rest args)
        (setf (tree-view-field-current-row-info (field-info-field field-info))
              tree-branches-data))
      view obj)
    (write-string 
      (weblocks::render-template-to-string 
        :tree-view-body-row-wt 
        (list :view view :widget widget :object obj)
        :content (weblocks::capture-weblocks-output (apply #'render-table-view-body-row view (getf obj :item) widget args))
        :prefix (weblocks::capture-weblocks-output (weblocks::safe-apply (sequence-view-row-prefix-fn view) view obj args))
        :row-class (if alternp "altern" nil)
        :children-content (weblocks::capture-weblocks-output
                            (loop for i in (getf obj :children)
                                  for j from 0 do 
                                  (with-table-view-body-row 
                                    view i widget 
                                    :level (1+ level) 
                                    :lastp (= j (1- (length (getf obj :children))))
                                    :single-child-p (= (length (getf obj :children)) 1)
                                    :lastp-map (append lastp-map (list (= j (1- (length (getf obj :children))))))
                                    :levels-count levels-count)))
        :suffix (weblocks::capture-weblocks-output (weblocks::safe-apply (sequence-view-row-suffix-fn view) view obj args)))
      *weblocks-output-stream*)))

(defmethod with-table-view-header-row :around ((view tree-view) obj widget &rest args)
  (call-next-method view (if (listp obj) 
                           (getf obj :item)
                           obj) widget))

(defmethod render-object-view-impl ((obj sequence) (view tree-view) widget &rest args &key
                                                   (fields-prefix-fn (view-fields-default-prefix-fn view))
                                                   (fields-suffix-fn (view-fields-default-suffix-fn view))
                                                   &allow-other-keys)
  (apply #'with-view-header view obj widget
         (lambda (view obj &rest args)
           (apply #'with-table-view-header view obj widget
                  (lambda (view obj widget &rest args)
                    (apply #'with-table-view-header-row view obj widget args))
                  (lambda (view obj widget &rest args)
                    (let ((row-num -1))
                      (let ((i 0))
                        (mapc (lambda (item)
                                (apply #'with-table-view-body-row view item
                                       widget
                                       :alternp (oddp (incf row-num))
                                       :lastp (= i (1- (length obj)))
                                       :single-child-p (= (length obj) 1)
                                       :lastp-map (list (= i (1- (length obj))))
                                       :levels-count 3
                                       args)
                                (incf i))
                              obj))))
                  args))
         args))

(defclass tree-branches-presentation (html-presentation)
  ((straight-column-captions :initform t :initarg :straight-column-captions)))

(defun tree-branches-presentation-field-value-wt (&key value level lastp single-child-p lastp-map levels-left straight-column-captions)
  (yaclml:with-yaclml-output-to-string
    (<:div :style "font-family:monospace;"
           (loop for j in lastp-map 
                 do 
                 (if j 
                   (<:as-is "&nbsp;")
                   (<:as-is "|"))
                 (<:as-is "&nbsp;&nbsp;"))
           (if lastp  
             (<:as-is "`--")
             (<:as-is "|--"))
           (when straight-column-captions
             (loop for i in levels-left do 
                   (if (getf i :firstp)
                     (<:as-is ".")
                     (<:as-is "-"))
                   (<:as-is "--")))
           (<:as-is value))))

(weblocks::deftemplate :tree-branches-presentation-field-value-wt 'tree-branches-presentation-field-value-wt)

(defmethod render-view-field-value (value (presentation tree-branches-presentation)
                                          field view widget obj &rest args
                                          &key &allow-other-keys)
  (let ((printed-value (apply #'print-view-field-value value presentation field view widget obj args)))
    (write-string 
      (apply #'weblocks::render-template-to-string 
             (list* :tree-branches-presentation-field-value-wt
                    (list :field field :view view :widget widget :object obj :presentation presentation)
                    :value printed-value 
                    :straight-column-captions (slot-value presentation 'straight-column-captions)
                    (tree-view-field-current-row-info field)))
      *weblocks-output-stream*)))
