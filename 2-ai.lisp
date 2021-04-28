;; the intent was for the ai not to make the impassable barriers, but it makes it useless. disable the barriers in mechanics somehow.

(defun auto-turn (player)
  (dotimes (x *w*)
    (dotimes (y *h*)
      (auto-found x y player)))
  (dotimes (x *w*)
    (dotimes (y *h*)
      (auto-move x y player)))
  (dotimes (x *w*)
    (dotimes (y *h*)
      (buy-unit x y player))))

(defun auto-found (x y player)
  (let ((unit (gethash (cons x y) *units*)))
    (cond (unit
           (when (equal (unit-player unit) player)
             (if (and (> (unit-moves unit) 0)
                      (not (some #'(lambda (num) (near-a-city? x y num))
                                 (countdown *player-count*))))
                 (found-city x y player)))))))

(defun auto-move (x y player)
  (let ((unit (gethash (cons x y) *units*)))
    (cond (unit
           (when (equal (unit-player unit) player)
             (move-unit-attempt x y 0 -1 player)
             (move-unit-attempt x y 0 1 player)
             (move-unit-attempt x y -1 0 player)
             (move-unit-attempt x y 1 0 player))))))
