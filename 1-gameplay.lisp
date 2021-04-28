#|
There are a lot of checks here that only arise when an AI calls (for example) found-city. The intent is to avoid an accidental cheating AI.
|#

(defstruct city
  (x 0)
  (y 0)
  (player 0)
  (people 1)
  (radius 1))

(defstruct player
  (id 0)
  (money 0)
  (primary-colour nil)
  (secondary-colour nil))

(defstruct unit
  (x 0)
  (y 0)
  (hp 3)
  (player 0)
  (max-moves 1)
  (moves 1))

(defparameter *basic-colour* "#abc")
(defparameter *primary-colours* (list "#00f"
                                      "#0f0"
                                      "#f00"))
(defparameter *secondary-colours* (list "#dd2"
                                        "#d2d"
                                        "#2dd"))
(defparameter *canvas-h* 500)
(defparameter *canvas-w* 500)
(defparameter *h* 10)
(defparameter *w* 10)
(defparameter *player-count* 2)
(defparameter *players* (let ((acc (ht)))
                          (dotimes (i *player-count*)
                            (setf (gethash i acc)
                                  (make-player :id i :money 0 :primary-colour (nth i *primary-colours*) :secondary-colour (nth i *secondary-colours*))))
                          acc))
(defparameter *unit-cost* 24)

;;; CONFIG
;;;----------------------------------------------------------------------------------------------
;;; GLOBALS

(defparameter *turn* 0)
(defparameter *units* (ht))
(defparameter *buildings* (ht))
(defparameter *cities* (ht))
(defparameter *units* (ht))

;;; GLOBALS
;;;----------------------------------------------------------------------------------------------
;;; READING GAME STATUS

(defun nearest-city (x y)
  ;; city distance unique?
  (best-key (map-to-hash #'(lambda (city) (dst (cons x y)
                                               (cons (city-x city)
                                                     (city-y city))))
                         (list-values *cities*))
            #'<=))

(defun field-colour (x y)
  (player-primary-colour (gethash (city-player (nearest-city x y))
                                  *players*)))

(defun money-info ()
  (mapcar #'(lambda (num) (list (player-money (gethash num *players*))
                                (apply #'+ (mapcar #'income (remove-if-not #'(lambda (city) (equal num (city-player city)))
                                                                           (list-values *cities*))))))
          (reverse (countdown *player-count*))))

(defun near-a-city? (x y player)
  (some #'(lambda (city)
            (and (equal (city-player city) player)
                 (<= (dst (cons x y)
                          (cons (city-x city)
                                (city-y city)))
                     (city-radius city))))
        (list-values *cities*)))

;;; READING GAME STATUS
;;;----------------------------------------------------------------------------------------------
;;; PROCESSING GAME DATA

(defun income (city)
  (let ((acc 0)
        (city-pos (cons (city-x city)
                        (city-y city))))
    (dotimes (x *w*)
      (dotimes (y *h*)
        (cond ((equal (cons x y)
                      city-pos)
               (incf acc (1+ (city-people city))))
              ((and (<= (dst city-pos (cons x y))
                        (city-radius city))
                    (equal (nearest-city x y)
                           city))
               (incf acc)))))
    acc))

;;; PROCESSING GAME DATA
;;;----------------------------------------------------------------------------------------------
;;; ACTIONS

(defun all-units (player fun)
  (dolist (unit (mapcar #'(lambda (coords) (gethash coords *units*))
                        (remove-if-not #'(lambda (tested-unit)
                                           (equal player (unit-player (gethash tested-unit *units*))))
                                       (list-keys *units*))))
    (funcall fun unit)))

(defun all-cities (player fun)
  (dolist (city (mapcar #'(lambda (coords) (gethash coords *cities*))
                        (remove-if-not #'(lambda (tested-city)
                                           (equal player (city-player (gethash tested-city *cities*))))
                                       (list-keys *cities*))))
    (funcall fun city)))

(defun kill (unit-loc)
  (remhash unit-loc *units*)
  (kill-graphics unit-loc))

(defun capture (city new-owner)
  (setf (city-player city) new-owner)
  (capture-graphics (cons (city-x city) (city-y city))
                    new-owner))

(defun move (unit dx dy)
  (print (concat "Moving from (" (unit-x unit) ", " (unit-y unit) ") by (" dx ", " dy ")."))
  (decf (unit-moves unit))
  (move-graphics (cons (unit-x unit) (unit-y unit)) dx dy)
  (remhash (cons (unit-x unit) (unit-y unit)) *units*)
  (incf (unit-x unit) dx)
  (incf (unit-y unit) dy)
  (setf (gethash (cons (unit-x unit) (unit-y unit))
                 *units*)
        unit))

(defun move-unit-attempt (x y dx dy player)
  (let* ((unit (gethash (cons x y) *units*))
         (target (cons (+ x dx) (+ y dy)))
         (target-unit (gethash target *units*))
         (target-city (gethash target *cities*)))
    (when (and (> *h* (+ x dx) -1)
               (> *w* (+ y dy) -1)
               unit
               (equal (unit-player unit) player)
               (> (unit-moves unit) 0))
      (if target-unit
          (if (not (equal (unit-player target-unit) player))
              (progn
                (kill (cons x y))
                (kill target)))
          (if target-city
              (when (not (equal player (city-player target-city)))
                (capture target-city player)
                (decf (unit-moves unit)))
              (move unit dx dy))))))

(defun found-city (x y player)
  (remhash (cons x y) *units*)
  (setf (gethash (cons x y) *cities*)
        (make-city :x x :y y :player player))
  (found-city-graphics (cons x y) player)
  (redraw))

(defun found-city-attempt (x y player)
  (let ((unit (gethash (cons x y) *units*)))
    (when (and unit
               (equal (unit-player unit) player)
               (> (unit-moves unit) 0)
               (not (some #'(lambda (num) (near-a-city? x y num))
                          (countdown *player-count*))))
      (found-city x y player))))

(defun next-turn ()
  (all-cities *turn* #'(lambda (city)
                         (incf (city-radius city) 0.025)))
  (all-units *turn* #'(lambda (unit)
                        (setf (unit-moves unit)
                              (unit-max-moves unit))))
  (redraw-unit-turns *turn*)
  
  (incf (player-money (gethash *turn* *players*))
        (apply #'+ (mapcar #'income (remove-if-not #'(lambda (city) (equal *turn* (city-player city)))
                                                   (list-values *cities*)))))
  (setf *turn* (mod (1+ *turn*) *player-count*)))

;;; ACTIONS
;;;----------------------------------------------------------------------------------------------
;;; MAIN

(defun tile-on-canvas (tile)
  (cons (* (+ (car tile) 1/2) (/ *canvas-h* *h*))
        (* (+ (cdr tile) 1/2) (/ *canvas-w* *w*))))

(let ((canvas-objects (ht))
      (tiles (ht))
      field
      money
      turn)
  
  (defun redraw-tile (canvas tile colour)
    (let ((center (tile-on-canvas tile)))
      (if (gethash tile tiles)
          (ltk:itemconfigure canvas (gethash tile tiles) :fill colour)
          (setf (gethash tile tiles)
                (square canvas center (/ *canvas-w* (* 2 *w*)) colour)))))
  
  (defun redraw ()
    (dotimes (x *h*)
      (dotimes (y *w*)
        (redraw-tile field (cons x y) (colour x y))))
    (setf (ltk:text money)
          (money-info))
    (setf (ltk:text turn)
          *turn*)
    (set-colour turn (player-primary-colour (gethash *turn* *players*))
                (player-secondary-colour (gethash *turn* *players*)))
    ;(ltk:after 0 #'(lambda () (ltk:mainloop)))
    )

  (defun redraw-unit-turns (player)
    (all-units player #'(lambda (unit)
                          (ltk:itemconfigure field
                                             (gethash (cons (unit-x unit)
                                                            (unit-y unit))
                                                      canvas-objects)
                                             :width (unit-outline unit))
                          (setf (unit-moves unit)
                                (unit-max-moves unit)))))

  (defun run ()
    (ltk:with-ltk ()
      (ltk:withdraw ltk:*tk*)
      (letrec ((W (window "civ"))
               (autoplay (button 1 1 W "Play for me" #'(lambda ()
                                                         (auto-turn *turn*)
                                                         (next-turn)
                                                         (redraw))))
               (next-turn (button 1 0 W "Next turn" #'(lambda ()
                                                        (next-turn)
                                                        (redraw)))))
        (setf field (canvas 0 0 W *canvas-h* *canvas-w*))
        (setf money (label 2 0 W (money-info)))
        (setf turn (label 3 0 W *turn*))
        (ltk:bind field "<Button-1>" #'(lambda (event)
                                         (action (floor (* *w* (/ (ltk:event-x event) *canvas-w*)))
                                                 (floor (* *h* (/ (ltk:event-y event) *canvas-h*))))))
        (ltk:bind field "<Motion>" (let ((last-active (cons 0 0)))
                                     #'(lambda (event)
                                         (let ((new-active (cons (floor (* *w* (/ (ltk:event-x event) *canvas-w*)))
                                                                 (floor (* *h* (/ (ltk:event-y event) *canvas-h*))))))
                                           (when (not (equal last-active new-active))
                                             (redraw-tile field last-active (colour (car last-active) (cdr last-active)))
                                             (redraw-tile field new-active (brighten (colour (car new-active) (cdr new-active))))
                                             (setf last-active new-active))))))
        
        (ltk:on-close W #'(lambda () (ltk:destroy ltk:*tk*)))
        (redraw)
        (found-city 0 0 0)
        (found-city 2 1 0)
        (found-city 1 3 0)
        (found-city (1- *h*) (1- *w*) 1))))
  
  (defun found-city-graphics (tile player)
    (if (gethash tile canvas-objects)
        (ltk:itemdelete field (gethash tile canvas-objects)))
    (setf (gethash tile canvas-objects)
          (square field (tile-on-canvas tile) 10 (player-secondary-colour (gethash player *players*))))
    (redraw))

  (defun capture-graphics (city-loc player)
    (ltk:itemconfigure field (gethash city-loc canvas-objects) :fill (player-secondary-colour (gethash player *players*)))
    (redraw))

  (defun kill-graphics (unit-loc)
    (ltk:itemdelete field (gethash unit-loc canvas-objects)))

  (defun unit-outline (unit)
    (1+ (* 3 (unit-moves unit))))

  (defun move-graphics (unit-loc dx dy)
    (let ((unit-obj (gethash unit-loc canvas-objects)))
      (move-hash canvas-objects unit-loc (add-coords unit-loc (cons dx dy)))
      (gethash (add-coords unit-loc (cons dx dy)) canvas-objects)
      (ltk:itemconfigure field unit-obj :width (unit-outline (gethash unit-loc *units*)))
      (if unit-obj
          (ltk:itemmove field unit-obj
                        (* dx (/ *canvas-h* *h*))
                        (* dy (/ *canvas-w* *w*)))
          (error (concat "Trying to move unit at (" (car unit-loc) ", " (cdr unit-loc) "), but there is no unit there.")))))

  (defun buy-graphics (unit-loc player)
    (setf (gethash unit-loc canvas-objects)
          (circle field (tile-on-canvas unit-loc) 10 (player-secondary-colour (gethash player *players*))))))

(defun colour (x y)
  (if (some #'(lambda (player) (near-a-city? x y player))
            (countdown *player-count*))
      (field-colour x y)
      *basic-colour*))

(defun create-unit (x y player)
  (setf (gethash (cons x y) *units*)
        (make-unit :x x :y y :player player :moves 0))
  (print (concat "Buying at (" x ", " y ")."))
  (buy-graphics (cons x y) player))

(defun buy-unit (x y player)
  (when (and (>= (player-money (gethash player *players*))
                 *unit-cost*)
             (null (gethash (cons x y) *units*))
             (null (gethash (cons x y) *cities*))
             (near-a-city? x y player)
             (equal player
                    (city-player (nearest-city x y))))
    (decf (player-money (gethash player *players*))
          *unit-cost*)
    (create-unit x y player)))

;;; MAIN
;;;----------------------------------------------------------------------------------------------
;;; MOVE ATTEMPTS (VALID ACTIONS)

(defun action (x y)
  (let* ((tile (cons x y))
         (city (gethash tile *cities*))
         (unit (gethash tile *units*)))
    (cond (city
           (print city))
          (unit
           (if (and (> (unit-moves unit) 0)
                    (equal *turn* (unit-player unit)))
               (let ((W (window "DIR")))
                 (button 1 0 W "<" #'(lambda () (ltk:destroy W) (move-unit-attempt x y -1 0 *turn*)))
                 (button 1 2 W ">" #'(lambda () (ltk:destroy W) (move-unit-attempt x y 1 0 *turn*)))
                 (button 0 1 W "^" #'(lambda () (ltk:destroy W) (move-unit-attempt x y 0 -1 *turn*)))
                 (button 2 1 W "v" #'(lambda () (ltk:destroy W) (move-unit-attempt x y 0 1 *turn*)))
                 (if (not (some #'(lambda (num) (near-a-city? x y num))
                                (countdown *player-count*)))
                     (button 1 1 W "o" #'(lambda () (ltk:destroy W) (found-city x y *turn*)))))))
          
          ((and (near-a-city? x y *turn*)
                (equal *turn*
                       (city-player (nearest-city x y))))
           (let ((W (window "SHOP")))
             (button 0 0 W "X" #'(lambda () (ltk:destroy W)))
             (if (>= (player-money (gethash *turn* *players*))
                     *unit-cost*)
                 (button 1 0 W "Buy unit" #'(lambda ()
                                              (buy-unit x y *turn*)
                                              (ltk:destroy W)))))))))
