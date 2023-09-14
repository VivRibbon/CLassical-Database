(defclass piece ()
  ((title
    :initarg :title
    :accessor title)
   (composer
    :initarg :composer
    :accessor composer)
   (era
    :initarg :era
    :initform nil
    :accessor era)
   (form
    :initarg :form
    :initform nil
    :accessor form)
   (performances
    :initarg :performances
    :initform ()
    :accessor performances)))

(defclass performance ()
  ((conductor
    :initarg :conductor
    :initform "None"
    :accessor conductor)
   (performers
    :initarg :performers
    :accessor performers)
   (rating
    :initarg :rating
    :initform "N/A"
    :accessor rating)))

(defmethod print-object ((obj piece) stream)
      (print-unreadable-object (obj stream :type t)
        (with-accessors ((title title)
                         (composer composer)
                         (era era)
                         (form form)
                         (performances performances))
            obj
          (format stream "~%Title: ~a~%Composer: ~a~%Era: ~a~%Form: ~a~%Performances: ~a" title composer era form performances))))

(defmethod print-object ((obj performance) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((performers performers)
                     (conductor conductor)
                     (rating rating))
        obj
      (format stream "~%Performers: ~a, Conductor: ~a, Rating: ~a~%" performers conductor rating))))

(defun make-piece (title composer &key era form performances)
  (make-instance `piece :title title :composer composer :era era :form form :performances performances))

(defun make-performance (performers &key conductor rating)
  (make-instance `performance :conductor conductor :performers performers :rating rating))

(defun make-entry (title composer &key era form performances performers conductor rating)
  (make-piece title composer :era era :form form
                             :performances (list (funcall
                                                  #'make-performance performers
                                                  :conductor conductor
                                                  :rating rating))))

(defvar *db* nil)

(defun add-entry (piece) (push piece *db*))

(defun save-db (filename)
 (with-open-file (out filename
                      :direction :output
                      :if-exists :supersede)
   (with-standard-io-syntax
     (print *db* out))))

(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))
