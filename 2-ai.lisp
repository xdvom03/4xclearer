(defun ny-distance (tile1 tile2)
  (+ (abs (- (car tile1)
             (car tile2)))
     (abs (- (cdr tile1)
             (cdr tile2)))))

(defun random-pick (lst)
  (nth (random (length lst)) lst))

(defun weighted-pick (lst weight-fun)
  (let* ((score-left (reduce #'+ (mapcar weight-fun lst)))
         (acc nil))
    (dolist (elem lst)
      (if (null acc)
          (if (chance (/ (funcall weight-fun elem) score-left))
              (setf acc elem)
              (decf score-left (funcall weight-fun elem)))))
    acc))

(defun flippable? (tile player)
  (let ((acc nil))
    (mapcar #'(lambda (player)
                (all-cities player #'(lambda (city)
                                       (if (<= (dst (city-pos city) tile)
                                               (city-radius city))
                                           (push player acc)))))
            (countdown *player-count*))
    (let* ((player-score (count player acc))
           (opponent-scores (map-to-hash #'(lambda (pl)
                                             (count pl acc))
                                         (remove-if #'(lambda (pl)
                                                        (equal pl player))
                                                    (countdown *player-count*))))
           (best-opponent-score (gethash (best-key opponent-scores #'>) opponent-scores)))
      (or (= player-score best-opponent-score) ; gain a neutral one
          ;; neutralise an opponent one (must already be one)
          (and (= (1+ player-score) best-opponent-score)
               (tile-owner (car tile) (cdr tile)))))))

(defun neighbours (tile)
  (remove-if-not #'(lambda (tile2)
                     (= (dst tile2 tile) 1))
                 (all-tiles)))

(defun income-change (tile player)
  ;; We do get the tile itself plus however many of its neighbours we get to flip. One is automatic for the city center.
  (let ((neighbours (neighbours tile)))
    (1+ (length (remove-if-not #'(lambda (tile2)
                                   (flippable? tile2 player))
                           neighbours)))))

(defun target-score (tile player target)
  (* (expt (1+ (ny-distance tile target)) -4)
     (expt (income-change target player) 6)))

(defun near-a-city? (x y player)
  (some #'(lambda (city)
            (and (equal (city-player city) player)
                 (<= (dst (cons x y)
                          (cons (city-x city)
                                (city-y city)))
                     (city-radius city))))
        (list-values *cities*)))

(defun invalid-target-tile? (tile player existing-targets &optional allow-redundancy?)
  ;; TBD: This could be a score. Also, Boolean mess.
  ;; Remove all captured territory
  (or (and (some #'(lambda (player)
                     (near-a-city? (car tile) (cdr tile) player))
                 (countdown *player-count*))
           ;; Except others' cities
           (not (and (gethash tile *cities*)
                     (not (equal player (city-player (gethash tile *cities*)))))))
      ;; Then remove everything already targeted
      (if allow-redundancy?
          nil
          (some #'(lambda (target)
                    (<= (ny-distance target tile) 1))
                existing-targets))))

(defun tile-score (tile player)
  ;; How good it would be to start there?
  (reduce #'+ (mapcar #'(lambda (target)
                          (target-score tile player target))
                      (useful-tiles player))))

(defun useful-tiles (player)
  ;; must re-check existing targets first
  (let* ((existing-targets (let ((acc nil))
                             (all-units player #'(lambda (unit)
                                                   (if (unit-target unit)
                                                       (push (unit-target unit) acc))))
                             acc))
         (useful-tiles (remove-if #'(lambda (tile)
                                      (invalid-target-tile? tile player existing-targets))
                                  (all-tiles))))
    (fallback useful-tiles
              (remove-if #'(lambda (tile)
                             (invalid-target-tile? tile player existing-targets t))
                         (all-tiles)))))

(defun new-target (origin player)
  (let ((choice (weighted-pick (fallback (useful-tiles player)
                                         (error "Did we win?"))
                               #'(lambda (target)
                                   (target-score origin player target)))))
    choice))

(defun found-sanity-check (x y player)
  (let ((guard-tile (find-if #'(lambda (tile) (let ((unit (gethash tile *units*)))
                                                (and unit (not (equal (unit-player unit) player)))))
                             (neighbours (cons x y)))))
    (if guard-tile
        (move-unit-attempt x y (- (car guard-tile) x) (- (cdr guard-tile) y) player)
        (found-city-attempt x y player))))

(defun auto-turn (player)
  ;; Start by setting new targets if old ones have become invalid
  (dotimes (x *w*)
    (dotimes (y *h*)
      (let ((unit (gethash (cons x y) *units*)))
        (when (and unit
                   (equal (unit-player unit) player))
          (let ((target (unit-target unit)))
            (if (and target
                     (invalid-target-tile? target player nil)) ;; we only care about targets made actually invalid, not redundant
                (progn
                  (setf (unit-target unit)
                        (new-target (cons x y) player))
                  (if *show-target-lines?*
                      (reset-target-line unit)))))))))
  (dotimes (x *w*)
    (dotimes (y *h*)
      (let ((unit (gethash (cons x y) *units*)))
        (when (and unit
                   (equal (unit-player unit) player))
          (let ((target (unit-target unit)))
            (if target
                (multiple-value-bind (x-dir y-dir)
                    (move-to-target (cons x y) target)
                  (if (zerop x-dir)
                      (if (zerop y-dir)
                          (found-sanity-check x y player) ;; we got to the target!
                          (move-unit-attempt x y 0 y-dir player))
                      (if (zerop y-dir)
                          (move-unit-attempt x y x-dir 0 player)
                          (progn
                            (if (chance 1/2)
                                (move-unit-attempt x y x-dir 0 player))
                            (move-unit-attempt x y 0 y-dir player)
                            (move-unit-attempt x y x-dir 0 player)))))
                (progn
                  (move-unit-attempt x y 0 -1 player)
                  (move-unit-attempt x y 0 1 player)
                  (move-unit-attempt x y -1 0 player)
                  (move-unit-attempt x y 1 0 player))))))))
  (dotimes (i (* *h* *w*))
    (if (>= (player-money (gethash player *players*))
            *unit-cost*)
        (let* ((buy-place (weighted-pick (remove-if-not #'(lambda (tile)
                                                            (near-a-city? (car tile) (cdr tile) player))
                                                        (all-tiles))
                                         #'(lambda (tile)
                                             (tile-score tile player))))
               (target (new-target buy-place player)))
          (buy-unit (car buy-place) (cdr buy-place) player target)))))

(defun move-to-target (start end)
  ;; +-1, first for x, then for y.
  (values (signum (- (car end) (car start)))
          (signum (- (cdr end) (cdr start)))))

(defun append1 (lst elem)
  (append lst (list elem)))
