(defthing keeper thing (id (gensym "kept")) (_cache))

(defmacro keep (it &body body)
 "using the hash table _cache, compute this once, then keep it"
 (let ((val     (gensym))
       (found-p (gensym))
       (key     (gensym)))
  `(with-slots (_cache) ,it
	  (setf _cache (or _cache
			(make-hash-table)))
	  (multiple-value-bind (,val ,found-p)
	   (gethash ',key _cache)
	   (if ,found-p ,val
	    (setf (gethash ',key _cache)
	     (progn ,@body)))))))

(defmacro defmethod! (m args &body b)
 (if (stringp (car b))
  `(defmethod ,m (,@args) ,(car b) (keep ,(caar args) ,@(cdr b)))
  `(defmethod ,m (,@args)          (keep ,(caar args) ,@b))))
