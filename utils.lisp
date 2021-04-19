(ql:quickload (list "ltk" "ltk-mw"))

(defun pass ())

(defun warning-box (text title)
  (ltk:message-box text title "ok" "warning"))

(defun info-box (text title)
  (ltk:message-box text title "ok" "info"))

(defun ht ()
  (make-hash-table :test #'equal))

(defmacro letrec (bindings &body decls/forms)
  (assert (and (listp bindings)
               (every #'(lambda (b)
                          (or (symbolp b)
                              (and (consp b)
                                   (symbolp (first b))
                                   (null (cddr b)))))
                      bindings))
          (bindings) "malformed bindings")
  (let* ((names (mapcar #'(lambda (b)
                            (etypecase b
                              (symbol b)
                              (cons (first b))))
                        bindings))
         (values (mapcar #'(lambda (b)
                             (etypecase b
                               (symbol nil)
                               (cons (second b))))
                         bindings))
         (nvpairs (reduce #'append (mapcar #'list names values))))
    `(let ,names
       (setf ,@nvpairs)
       (locally
           ,@decls/forms))))

(defun sq (a)
  (expt a 2))

(defun dst (field1 field2)
  (sqrt (+ (sq (- (car field1)
                  (car field2)))
           (sq (- (cdr field1)
                  (cdr field2))))))

(defun list-hashes (hashtable)
  (let ((acc nil))
    (maphash #'(lambda (a b) (push (cons a b) acc))
             hashtable)
    acc))

(defun list-keys (hashtable)
  (let ((acc nil))
    (maphash #'(lambda (a b) (declare (ignore b)) (push a acc))
             hashtable)
    acc))

(defun list-values (hashtable)
  (let ((acc nil))
    (maphash #'(lambda (a b) (declare (ignore a)) (push b acc))
             hashtable)
    acc))

(defun countdown (num)
  ;; Returns a list of length num with integers decrementing towards zero
  (let ((acc nil))
    (dotimes (i num)
      (push i acc))
    acc))

(defun best-key (hashtable pred)
  (best-element (list-keys hashtable) pred #'(lambda (el) (gethash el hashtable))))

(defun best-element (lst pred key)
  (if lst
      (let* ((acc (car lst))
             (max (funcall key acc))
             (unique? t))
        (dolist (i (cdr lst))
          (if (funcall pred (funcall key i) max)
              (progn
                (setf unique? (not (funcall pred max (funcall key i))))
                (setf acc i)
                (setf max (funcall key acc)))))
        (values acc max unique?))))

(defun map-to-hash (fun list &key key-fun)
  ;; key-fun is applied to the list to produce keys.
  (let ((acc (make-hash-table :test #'equal)))
    (dolist (elem list)
      (setf (gethash (if key-fun
                         (funcall key-fun elem)
                         elem)
                     acc)
            (funcall fun elem)))
    acc))

(defun concat (&rest strings)
  ;; Writes numbers out, but leaves the rest be to signal errors if something VERY wrong is supplied
  ;; Not allowing lists for now because it seems like more trouble than it's worth
  (apply #'concatenate 'string (mapcar #'(lambda (a)
                                           (if (null a) (error "Trying to concat with NIL")) ; specific case, signal first
                                           (if (listp a) (error "Trying to concat a list, use convert-to-str instead!"))
                                           (if (numberp a)
                                               (write-to-string a)
                                               a))
                                       strings)))

;;; GENERAL
;;;----------------------------------------------------------------------------------------------
;;; GUI

(defun widget (r c type master)
  (let ((w (make-instance type :master master)))
    (ltk:grid w r c :sticky "nesw")
    (ltk:grid-columnconfigure master c :weight 1)
    (ltk:grid-rowconfigure master r :weight 1)
    w))

(defun button (r c master txt command)
  (let ((b (widget r c 'ltk:button master)))
    (setf (ltk:text b) txt)
    (setf (ltk:command b) command)
    (ltk:configure b :font "NotoSans 24")
    b))

(defun listbox (r c master txt)
  (let ((l (widget r c 'ltk:listbox master)))
    (setf (ltk:text l) txt)
    l))

(defun entry (r c master)
  (widget r c 'ltk:entry master))

(defun checkbox (r c master text command)
  (let ((ch (widget r c 'ltk:check-button master)))
    (setf (ltk:command ch) command)
    (setf (ltk:text ch) text)
    ch))

(defun label (r c master txt)
  (let ((l (widget r c 'ltk:label master)))
    (setf (ltk:text l) txt)
    (ltk:configure l :anchor :center)
    l))

(defun frame (r c master)
  ;; Ugly hack: LTK does not support backround colours of frames, but it works for canvases, and they seem to work serviceably as frames. 
  (let ((f (widget r c 'ltk:canvas master)))
    f))

(defun window (title)
  (let ((W (make-instance 'ltk:toplevel :title title)))
    W))

(defun set-colour (widget colour1 colour2)
  (ltk:configure widget :background colour1)
  (ltk:configure widget :foreground colour2))
