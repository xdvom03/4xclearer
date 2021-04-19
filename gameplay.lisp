(defstruct city
  (x-pos 0)
  (y-pos 0)
  (player 0)
  (people 1)
  (radius 1))

(defstruct player
  (id 0)
  (money 0)
  (primary-colour nil)
  (secondary-colour nil))

(defstruct unit
  (x-pos 0)
  (y-pos 0)
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

(defun nearest-city (i j)
  ;; city distance unique?
  (best-key (map-to-hash #'(lambda (city) (dst (cons i j)
                                               (cons (city-x-pos city)
                                                     (city-y-pos city))))
                         (list-values *cities*))
            #'<=))

(defun field-colour (i j)
  (player-primary-colour (gethash (city-player (nearest-city i j))
                                  *players*)))

(defun money-info ()
  (mapcar #'(lambda (num) (list (player-money (gethash num *players*))
                                (apply #'+ (mapcar #'income (remove-if-not #'(lambda (city) (equal num (city-player city)))
                                                                           (list-values *cities*))))))
          (reverse (countdown *player-count*))))

(defun near-a-city? (i j player)
  (some #'(lambda (city)
            (and (equal (city-player city) player)
                 (<= (dst (cons i j)
                          (cons (city-x-pos city)
                                (city-y-pos city)))
                     (city-radius city))))
        (list-values *cities*)))

;;; READING GAME STATUS
;;;----------------------------------------------------------------------------------------------
;;; PROCESSING GAME DATA

(defun income (city)
  (let ((acc 0)
        (city-pos (cons (city-x-pos city)
                        (city-y-pos city))))
    (dotimes (i *h*)
      (dotimes (j *w*)
        (cond ((equal (cons i j)
                      city-pos)
               (incf acc (1+ (city-people city))))
              ((and (<= (dst city-pos (cons i j))
                        (city-radius city))
                    (equal (nearest-city i j)
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

(defun move-unit (i j dx dy)
  (let* ((unit (gethash (cons i j) *units*))
         (target (cons (+ i dx) (+ j dy)))
         (target-unit (gethash target *units*))
         (target-city (gethash target *cities*)))
    (when (and (> *h* (+ i dx) -1)
               (> *w* (+ j dy) -1))
      (if target-unit
          (if (not (equal (unit-player target-unit) *turn*))
              (progn
                (remhash (cons i j) *units*)
                (remhash target *units*)))
          (progn
            (if target-city
                (when (not (equal *turn* (city-player target-city)))
                  (setf (city-player target-city) *turn*)
                  (decf (unit-moves unit)))
                (progn
                  (decf (unit-moves unit))
                  (setf (unit-x-pos unit) (+ i dx))
                  (setf (unit-y-pos unit) (+ j dy))
                  (remhash (cons i j) *units*)
                  (setf (gethash (cons (+ i dx)
                                       (+ j dy))
                                 *units*)
                        unit))))))))

(defun found-city (i j player)
  (remhash (cons i j) *units*)
  (setf (gethash (cons i j) *cities*)
        (make-city :x-pos i :y-pos j :player player)))

(defun next-turn ()
  (all-cities *turn* #'(lambda (city)
                         (incf (city-radius city) 0.025)))
  (all-units *turn* #'(lambda (unit)
                        (setf (unit-moves unit)
                              (unit-max-moves unit))))
  
  (incf (player-money (gethash *turn* *players*))
        (apply #'+ (mapcar #'income (remove-if-not #'(lambda (city) (equal *turn* (city-player city)))
                                                   (list-values *cities*)))))
  (setf *turn* (mod (1+ *turn*) *player-count*)))

;;; ACTIONS
;;;----------------------------------------------------------------------------------------------
;;; MAIN

(defun text (i j)
  (cond ((gethash (cons i j) *cities*)
         (concat "C" (income (gethash (cons i j) *cities*))))
        ((gethash (cons i j) *units*)
         (concat "U" (unit-moves (gethash (cons i j) *units*))))
        (t "")))

(defun field-hash (h w fun)
  (let ((acc (ht)))
    (dotimes (i h)
      (dotimes (j w)
        (let ((ii i)
              (jj j))
          (setf (gethash (cons i j) acc)
                (funcall fun ii jj)))))
    acc))

(defun redraw (field)
  (dotimes (i *h*)
    (dotimes (j *w*)
      (let ((button (gethash (cons i j) field))
            (unit (gethash (cons i j) *units*))
            (city (gethash (cons i j) *cities*)))
        (setf (ltk:text button)
              (text i j))
        (set-colour button (colour i j)
                    (if unit
                        (player-secondary-colour (gethash (unit-player unit) *players*))
                        (if city
                            (player-secondary-colour (gethash (city-player city) *players*))
                            "#000")))))))

(defun run ()
  (found-city 0 0 0)
  (found-city 2 1 0)
  (found-city 1 3 0)
  (found-city (1- *h*) (1- *w*) 1)
  (ltk:with-ltk ()
    (ltk:withdraw ltk:*tk*)
    (letrec ((W (window "civ"))
             (field-frame (frame 0 0 W))
             (field (field-hash *h* *w* #'(lambda (i j) (button i j field-frame (text i j) #'(lambda ()
                                                                                               (action i j)
                                                                                               (redraw field)
                                                                                               (setf (ltk:text money)
                                                                                                     (money-info))
                                                                                               (setf (ltk:text turn)
                                                                                                     *turn*))))))
             (money (label 2 0 W (money-info)))
             (turn (label 3 0 W *turn*))
             (autoplay (button 1 1 W "Play for me" #'(lambda ()
                                                       (auto-turn *turn*)
                                                       (next-turn)
                                                       (redraw field)
                                                       (setf (ltk:text money)
                                                             (money-info))
                                                       (setf (ltk:text turn)
                                                             *turn*)
                                                       (set-colour turn (player-primary-colour (gethash *turn* *players*))
                                                                   (player-secondary-colour (gethash *turn* *players*))))))
             (next-turn (button 1 0 W "Next turn" #'(lambda ()
                                                      (next-turn)
                                                      (redraw field)
                                                      (setf (ltk:text money)
                                                            (money-info))
                                                      (setf (ltk:text turn)
                                                            *turn*)
                                                      (set-colour turn (player-primary-colour (gethash *turn* *players*))
                                                                  (player-secondary-colour (gethash *turn* *players*)))))))
      (ltk:on-close W #'(lambda () (ltk:destroy ltk:*tk*)))
      (setf (ltk:text money)
            (money-info))
      (setf (ltk:text turn)
            *turn*)
      (redraw field)
      (set-colour turn (player-primary-colour (gethash *turn* *players*))
                  (player-secondary-colour (gethash *turn* *players*))))))

(defun colour (i j)
  (if (some #'(lambda (player) (near-a-city? i j player))
            (countdown *player-count*))
      (field-colour i j)
      *basic-colour*))

(defun action (i j)
  (let ((city (gethash (cons i j) *cities*))
        (unit (gethash (cons i j) *units*)))
    (cond (city
           (print city))
        
          (unit
           (if (and (> (unit-moves unit) 0)
                    (equal *turn* (unit-player unit)))
               (let ((W (window "DIR")))
                 (button 1 0 W "<" #'(lambda () (ltk:destroy W) (move-unit i j 0 -1)))
                 (button 1 2 W ">" #'(lambda () (ltk:destroy W) (move-unit i j 0 1)))
                 (button 0 1 W "^" #'(lambda () (ltk:destroy W) (move-unit i j -1 0)))
                 (button 2 1 W "v" #'(lambda () (ltk:destroy W) (move-unit i j 1 0)))
                 (if (not (some #'(lambda (num) (near-a-city? i j num))
                                (countdown *player-count*)))
                     (button 1 1 W "o" #'(lambda () (ltk:destroy W) (found-city i j *turn*)))))))
        
          ((and (near-a-city? i j *turn*)
                (equal *turn*
                       (city-player (nearest-city i j))))
           (let ((W (window "SHOP")))
             (button 0 0 W "X" #'(lambda () (ltk:destroy W)))
             (if (>= (player-money (gethash *turn* *players*))
                     *unit-cost*)
                 (button 1 0 W "Buy unit" #'(lambda ()
                                              (buy-unit i j *turn*)
                                              (ltk:destroy W)))))))))

(defun buy-unit (i j player)
  (decf (player-money (gethash player *players*))
        *unit-cost*)
  (setf (gethash (cons i j) *units*)
        (make-unit :x-pos i :y-pos j :player *turn* :moves 0)))
