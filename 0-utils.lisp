(ql:quickload (list "ltk" "ltk-mw"))

(defun pass ())

(defun warning-box (text title)
  (ltk:message-box text title "ok" "warning"))

(defun info-box (text title)
  (ltk:message-box text title "ok" "info"))

(defun ht ()
  (make-hash-table :test #'equal))

(defun move-hash (hash-table original-key new-key)
  ;; destructive (which is the point)
  ;; hash tables have limited test options, all of which are commutative, so we need not worry about the order here
  (multiple-value-bind (value exists?)
      (gethash original-key hash-table)
    (if (and exists?
             (not (funcall (hash-table-test hash-table) original-key new-key)))
        (progn
          (setf (gethash new-key hash-table)
                value)
          (remhash original-key hash-table)
          hash-table))))

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

(defun canvas (r c master h w)
  ;; Ugly hack: LTK does not support backround colours of frames, but it works for canvases, and they seem to work serviceably as frames. 
  (let ((canvas (widget r c 'ltk:canvas master)))
    (ltk:configure canvas :height h)
    (ltk:configure canvas :width w)
    canvas))

(defun window (title)
  (let ((W (make-instance 'ltk:toplevel :title title)))
    W))

(defun set-colour (widget colour1 colour2)
  (ltk:configure widget :background colour1)
  (ltk:configure widget :foreground colour2))

(defun circle (canvas center r colour)
  (let* ((x (car center))
         (y (cdr center))
         (circle (ltk:create-oval canvas
                                  (- x r)
                                  (- y r)
                                  (+ x r)
                                  (+ y r))))
    (ltk:itemconfigure canvas circle :fill colour)
    circle))

(defun square (canvas center r colour)
  (let* ((x (car center))
         (y (cdr center))
         (square (ltk:create-rectangle canvas
                                       (- x r)
                                       (- y r)
                                       (+ x r)
                                       (+ y r))))
    (ltk:itemconfigure canvas square :fill colour)
    square))

(defun horizontal-line (canvas y x1 x2)
  (ltk:create-line canvas (list x1 y x2 y)))

(defun vertical-line (canvas x y1 y2)
  (ltk:create-line canvas (list x y1 x y2)))

(defun brighten (colour)
  ;; expects three-digit RGBs
  ;; TBD: Horrifying hack!
  (let ((old-base *read-base*))
    (setf *read-base* 16)
    (let* ((avg-col (floor (sqrt (/ (+ (sq (read-from-string (subseq colour 1 2)))
                                       (sq (read-from-string (subseq colour 2 3)))
                                       (sq (read-from-string (subseq colour 3))))
                                    9))))
           (acc (concat "#" (write-to-string (+ (read-from-string (subseq colour 1))
                                                (read-from-string (concat (write-to-string (min (1+ avg-col) (- 15 (read-from-string (subseq colour 1 2)))))
                                                                          (write-to-string (min (1+ avg-col) (- 15 (read-from-string (subseq colour 2 3)))))
                                                                          (write-to-string (min (1+ avg-col) (- 15 (read-from-string (subseq colour 3))))))))
                                             :base 16))))
      (setf *read-base* old-base)
      acc)))

(defun add-coords (c1 c2)
  (cons (+ (car c1)
           (car c2))
        (+ (cdr c1)
           (cdr c2))))

(defun chance (odds)
  (< (random (expt 10 6))
     (* (expt 10 6)
        odds)))
