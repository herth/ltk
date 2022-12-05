(ql:quickload :ltk)
(in-package :ltk)

;;;; Basic tests

(defun save-text-test ()
  "Basic app to test text saving & loading."
  (let* ((path (make-pathname :name "ltk-save-text-test" :type "txt"
                              :directory (list :absolute :home))))
    (with-ltk ()
      (let* ((frame (make-instance 'frame
                                   :master nil
                                   :name "frame"))
             (text (make-instance 'text
                                  :master frame
                                  :name "text"))
             (save (make-instance 'button
                                  :master frame
                                  :name "save"
                                  :text "Save"
                                  :command
                                  (lambda ()
                                    (save-text text
                                               (namestring path)))))
             (load (make-instance 'button
                                  :master frame
                                  :name "load"
                                  :text "Load"
                                  :command
                                  (lambda ()
                                    (load-text text
                                               (namestring path))))))
        (pack frame)
        (pack text)
        (pack save) (pack load)))))
