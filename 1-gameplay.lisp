#|
There are a lot of checks here that only arise when an AI calls (for example) found-city. The intent is to avoid an accidental cheating AI.
|#

(defstruct city
  (x 0)
  (y 0)
  (player 0)
  (people 0)
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
  (moves 1)
  (target nil)
  (target-line nil))

(defparameter *show-target-lines?* t)
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

(defun unit-outline (unit)
  (1+ (* 3 (unit-moves unit))))

;;; GLOBALS
;;;----------------------------------------------------------------------------------------------
;;; READING GAME STATUS

(defun tile-owner (x y)
  (let ((acc nil))
    (mapcar #'(lambda (player)
                (all-cities player #'(lambda (city)
                                       (if (<= (dst (city-pos city) (cons x y))
                                               (city-radius city))
                                           (push player acc)))))
            (countdown *player-count*))
    (multiple-value-bind (best-pl max unique?)
        (best-element (countdown *player-count*) #'>= #'(lambda (pl) (count pl acc)))
      (declare (ignore max))
      (if unique?
          best-pl))))

(defun all-tiles ()
  (let ((acc nil))
    (dotimes (x *w*)
      (dotimes (y *h*)
        (push (cons x y) acc)))
    (reverse acc)))

(defun income (player)
  (length (remove-if-not #'(lambda (tile)
                             (equal (tile-owner (car tile) (cdr tile))
                                    player))
                         (all-tiles))))

(defun money-info ()
  (mapcar #'(lambda (player) (list (player-money (gethash player *players*))
                                   (income player)))
          (reverse (countdown *player-count*))))

;;; READING GAME STATUS
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
  (kill-graphics unit-loc)
  (remhash unit-loc *units*))

(defun capture (unit city)
  (decf (unit-moves unit))
  (setf (city-player city) (unit-player unit))
  (capture-graphics unit city))

(defun unit-pos (unit)
  (cons (unit-x unit)
        (unit-y unit)))

(defun city-pos (city)
  (cons (city-x city)
        (city-y city)))

(defun move (unit dx dy)
  (print (concat "Moving from (" (unit-x unit) ", " (unit-y unit) ") by (" dx ", " dy ")."))
  (decf (unit-moves unit))
  (move-graphics unit dx dy)
  (remhash (unit-pos unit) *units*)
  (incf (unit-x unit) dx)
  (incf (unit-y unit) dy)
  (setf (gethash (unit-pos unit)
                 *units*)
        unit)
  (if *show-target-lines?*
      (reset-target-line unit)))

(defun move-unit-attempt (x y dx dy player)
  (let* ((unit (gethash (cons x y) *units*))
         (target (cons (+ x dx) (+ y dy)))
         (target-unit (gethash target *units*))
         (target-city (gethash target *cities*)))
    (when (and (tile-on-board? (add-coords (cons x y) (cons dx dy)))
               unit
               (equal (unit-player unit) player)
               (> (unit-moves unit) 0))
      (if target-unit
          (if (not (equal (unit-player target-unit) player))
              (progn
                (kill (cons x y))
                (kill target)))
          (if target-city
              (if (equal player (city-player target-city))
                  (move-unit-attempt x y (* 2 dx) (* 2 dy) player)
                  (capture unit target-city))
              (move unit dx dy))))))

(defun found-city (x y player)
  (setf (gethash (cons x y) *cities*)
        (make-city :x x :y y :player player))
  (found-city-graphics (cons x y) player)
  (remhash (cons x y) *units*)
  (redraw))

(defun found-city-attempt (x y player)
  (let ((unit (gethash (cons x y) *units*)))
    (when (and unit
               (equal (unit-player unit) player)
               (> (unit-moves unit) 0)
               (null (tile-owner x y)))
      (found-city x y player))))

(defun next-turn ()
  (all-cities *turn* #'(lambda (city)
                         (incf (city-radius city) 0)))
  (all-units *turn* #'(lambda (unit)
                        (setf (unit-moves unit)
                              (unit-max-moves unit))))
  (redraw-unit-turns *turn*)
  
  (incf (player-money (gethash *turn* *players*))
        (income *turn*))
  (setf *turn* (mod (1+ *turn*) *player-count*)))

;;; ACTIONS
;;;----------------------------------------------------------------------------------------------
;;; MAIN

(defun tile-on-canvas (tile)
  (cons (* (+ (car tile) 1/2) (/ *canvas-h* *h*))
        (* (+ (cdr tile) 1/2) (/ *canvas-w* *w*))))

(defun tile-on-board? (tile)
  (and (> *w* (car tile) -1)
       (> *h* (cdr tile) -1)))

(let ((canvas-objects (ht))
      (tiles (ht))
      field
      money
      turn)
  
  (defun redraw-tile (canvas tile colour)
    (let ((center (tile-on-canvas tile)))
      (if (gethash tile tiles)
          (ltk:itemconfigure canvas (gethash tile tiles) :fill colour)
          (if (tile-on-board? tile)
              (setf (gethash tile tiles)
                    (square canvas center (/ *canvas-w* (* 2 *w*)) colour))))))
  
  (defun redraw ()
    (dotimes (x *h*)
      (dotimes (y *w*)
        (redraw-tile field (cons x y) (colour x y))))
    (setf (ltk:text money)
          (money-info))
    (setf (ltk:text turn)
          *turn*)
    (set-colour turn (player-primary-colour (gethash *turn* *players*))
                (player-secondary-colour (gethash *turn* *players*))))

  (defun redraw-unit-turns (player)
    (all-units player #'(lambda (unit)
                          (ltk:itemconfigure field
                                             (gethash (unit-pos unit)
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
        ;(found-city 2 1 0)
        (found-city (1- *h*) (1- *w*) 1))))
  
  (defun found-city-graphics (tile player)
    (let ((obj (gethash tile canvas-objects)))
      (if (and obj
               (gethash tile *units*)
               (unit-target-line (gethash tile *units*)))
          (ltk:itemdelete field (unit-target-line (gethash tile *units*))))
      (if obj
          (ltk:itemdelete field obj))
      (setf (gethash tile canvas-objects)
            (square field (tile-on-canvas tile) 10 (player-secondary-colour (gethash player *players*)))))
    (redraw))

  (defun capture-graphics (unit city)
    (ltk:itemconfigure field (gethash (unit-pos unit) canvas-objects) :width (unit-outline unit))
    (ltk:itemconfigure field (gethash (city-pos city) canvas-objects) :fill (player-secondary-colour (gethash (unit-player unit) *players*)))
    (redraw))

  (defun kill-graphics (unit-loc)
    (let ((unit (gethash unit-loc *units*)))
      (if (and unit (unit-target-line unit))
          (ltk:itemdelete field (unit-target-line unit))))
    (ltk:itemdelete field (gethash unit-loc canvas-objects)))

  (defun move-graphics (unit dx dy)
    (ltk:itemconfigure field (gethash (unit-pos unit) canvas-objects) :width (unit-outline unit))
    (if (gethash (unit-pos unit) canvas-objects)
        (ltk:itemmove field (gethash (unit-pos unit) canvas-objects)
                      (* dx (/ *canvas-h* *h*))
                      (* dy (/ *canvas-w* *w*)))
        (error (concat "Trying to move unit at (" (car (unit-pos unit)) ", " (cdr (unit-pos unit)) "), but there is no unit there.")))
    (move-hash canvas-objects (unit-pos unit) (add-coords (unit-pos unit) (cons dx dy))))

  (defun reset-target-line (unit)
    (if (unit-target-line unit)
        (progn
          (ltk:itemdelete field (unit-target-line unit))
          (setf (unit-target-line unit)
                (ltk:make-line field (append (conslist (tile-on-canvas (unit-pos unit)))
                                             (conslist (tile-on-canvas (unit-target unit)))))))))

  (defun buy-graphics (unit player target)
    (setf (gethash (unit-pos unit) canvas-objects)
          (circle field (tile-on-canvas (unit-pos unit)) 10 (player-secondary-colour (gethash player *players*))))
    (if (and target
             *show-target-lines?*)
        (setf (unit-target-line unit)
              (ltk:make-line field (append (conslist (tile-on-canvas (unit-pos unit)))
                                           (conslist (tile-on-canvas target))))))))

(defun conslist (cons)
  ;; BUG: maybe we should not use conses!
  (list (car cons)
        (cdr cons)))

(defun colour (x y)
  (if (tile-owner x y)
      (player-primary-colour (gethash (tile-owner x y)
                                      *players*))
      *basic-colour*))

(defun create-unit (x y player target)
  (print (concat "Buying at (" x ", " y ")."))
  (let ((unit (make-unit :x x :y y :player player :moves 0 :target target)))
    (setf (gethash (cons x y) *units*)
          unit)
    (buy-graphics unit player target)))

(defun buy-unit (x y player &optional target)
  (when (and (>= (player-money (gethash player *players*))
                 *unit-cost*)
             (null (gethash (cons x y) *units*))
             (null (gethash (cons x y) *cities*))
             (equal (tile-owner x y) player)
             (equal player
                    (tile-owner x y)))
    (decf (player-money (gethash player *players*))
          *unit-cost*)
    (create-unit x y player target)
    (redraw)))

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
                 (if (null (tile-owner x y))
                     (button 1 1 W "o" #'(lambda () (ltk:destroy W) (found-city x y *turn*)))))))
          
          ((equal *turn*
                  (tile-owner x y))
           (let ((W (window "SHOP")))
             (button 0 0 W "X" #'(lambda () (ltk:destroy W)))
             (if (>= (player-money (gethash *turn* *players*))
                     *unit-cost*)
                 (button 1 0 W "Buy unit" #'(lambda ()
                                              (buy-unit x y *turn*)
                                              (ltk:destroy W)))))))))
