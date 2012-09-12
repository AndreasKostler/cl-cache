;;;; cl-cache.lisp

(in-package #:cl-cache)

;;; "cl-cache" goes here. Hacks and glory await!

;; Defining hash-protocol

;; For simplicity we will assume cache is EQL Hash Table. This could easily be
;; extended later by providing functions for get, set, rem of vals and element count
;; or via a backend-mixing providing those methods.

(defclass cache ()
  ((cache :initarg :init-cache
          :initform (make-hash-table)
          :accessor cache)))


(defgeneric getval (key cache &optional default)
  (:documentation
"Finds the entry with KEY in CACHE and returns the associated value and T as multiple values, or returns DEFAULT and NIL if there is no such entry."))

(defgeneric (setf getval) (val key cache)
  (:documentation
"Modifies the value in cache associated with the given KEY, or adds a new entry."))

(defgeneric remval (key cache)
  (:documentation
"Remove the entry for KEY in CACHE.  Returns T if such an entry
existed; NIL otherwise."))

(defgeneric contains-p (key cache)
  (:documentation "Checks if CACHE contains a value associated with KEY."))

(defgeneric cache-count (cache)
  (:documentation
"Returns the number of entries in the given CACHE."))

(defmethod contains-p (key (cache cache))
  (nth-value 1 (gethash key (cache cache))))

(defmethod getval (key (cache cache) &optional default)
  (if (contains-p key cache)
      (gethash key (cache cache) default)
      (values nil default)))

(defmethod (setf getval) (val key (cache cache))
  (setf (gethash key (cache cache)) val))

(defmethod remval (key (cache cache))
  (remhash key (cache cache)))

(defmethod cache-count ((cache cache))
  (hash-table-count (cache cache)))

(defmethod seed ((cache cache) new-cache)
  (setf (cache cache) new-cache))

(defclass fn-cache (cache)
  ((fn :initform #'identity :initarg :fn :accessor cache-fn)))

(defmethod getval :around (key (cache fn-cache) &optional default)
  (multiple-value-bind (val exists-p)
      (call-next-method)
    (if exists-p
        (values (funcall (cache-fn cache) val) exists-p)
        (values val exists-p))))

(defclass fifo-cache (cache)
  ((q :initform (make-empty-q) :accessor fifo-q)
   (limit :initform 32 :reader fifo-limit)
   (count :initform 0 :accessor element-count)))

(defun make-fifo-cache (cache &key (limit 32))
  (make-instance 'fifo-cache :limit limit :init-cache cache))

(defmethod initialize-instance :after ((cache fifo-cache) &key init-cache limit)
  (setf (cache cache) (trim-cache init-cache limit)))

(defmethod cache-count ((cache fifo-cache))
  (element-count cache))

(defmethod (setf getval) :before (val key (cache fifo-cache))
  (when (>= (element-count cache) (fifo-limit cache))
    (let ((k (peek (fifo-q cache))))
      (remhash k (cache cache))
      (deq (fifo-q cache))
      (decf (element-count cache))))
  (unless (contains-p key cache)
    (enq (fifo-q cache) (list key))
    (incf (element-count cache))))

(defun remove-key-from-queue (key cache)
  (setf (fifo-q cache)
        (make-q-from-list (remove-if (lambda (el) (eql el key))
                                     (q-elements (fifo-q cache))))))

(defun trim-cache (cache limit)
  (let* ((count (hash-table-count cache))
         (el-count (if (> limit count) count limit)))
    (loop for key being the hash-keys in cache
          for i from 1 upto el-count
          with new-cache = (make-hash-table :size limit :test (hash-table-test cache))
          using (hash-value val)
          do (setf (gethash key new-cache) val)
          finally (return new-cache))))

(defmethod remval :before (key (cache fifo-cache))
  (if (contains-p key cache)
      (progn
        (remove-key-from-queue key cache)
        (decf (element-count cache)))))

(defclass ttl-cache (cache)
  ((ttl :reader ttl)
   (ttl-ms :initarg :ttl-ms :reader ttl-ms)))

(defmethod remval :after (key (cache ttl-cache))
  (remhash key (ttl cache)))

(defun remove-stale-entries (cache)
  (let ((ttl (ttl cache))
        (ttl-ms (ttl-ms cache)))
    (loop for key being the hash-keys in ttl
          using (hash-value val)
          do (progn
               (print (+ ttl-ms val))
               (print (get-internal-real-time))
               (print (> (get-internal-real-time) (+ ttl-ms val)))
               (when (> (get-internal-real-time) (+ ttl-ms val))
                 (remval key cache))))))

(defmethod contains-p (key (cache ttl-cache))
  (let ((ttl (gethash key (ttl cache) (ttl-ms cache))))
    (< (- (get-internal-real-time) ttl) (ttl-ms cache))))

(defmethod (setf getval) :before (val key (cache ttl-cache))
  (remove-stale-entries cache))

(defmethod (setf getval) :after (val key (cache ttl-cache))
  (setf (gethash key (ttl cache)) (get-internal-real-time)))

(defmethod initialize-instance :after ((cache ttl-cache) &key)
  (with-slots (ttl cache) cache
    (setf ttl (make-hash-table :test (hash-table-test cache)
                               :size (hash-table-count cache)))))


;; TODO: Make array based q
(defstruct q
  (last nil)
  (elements nil))

(defun make-empty-q () (make-q))

(defun make-q-from-list (list)
  (let ((q (make-empty-q)))
    (unless (null list)
      (setf (q-last q) (last list)
            (q-elements q) list))
  q))

(defun empty-q-p (q)
  (= (length (q-elements q)) 0))

(defun peek (q)
  (nth 0 (q-elements q)))

(defun deq (q)
  (when (listp (q-elements q))
    (pop (q-elements q))))

(defun enq (q items)
  (cond ((null items) nil)
        ((or (null (q-last q)) (null (q-elements q)))
         (setf (q-last q) (last items)
               (q-elements q) (nconc (q-elements q) items)))
        (t (setf (cdr (q-last q)) items
                 (q-last q) (last items)))))
