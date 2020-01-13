(defvar *canvas-width* 800)
(defvar *canvas-height* 600)
(defvar *cursor-position* (gamekit:vec2 0 0))
(defvar *cursor-pressed* nil)

(deftype render-mode ()
  '(member :DEFAULT :EARS :TRIANGULATION))

(defvar *render-mode* ':DEFAULT)

(defparameter *polygon* (polygon (vertex 100 100)))

(gamekit:defgame flatso-demo () ()
  (:viewport-width *canvas-width*)
  (:viewport-height *canvas-height*)
  (:viewport-title "flatso"))

(defmethod gamekit:post-initialize ((app flatso-demo))
  (gamekit:bind-cursor (lambda (x y)
                         "Save cursor position"
                         (setf (gamekit:x *cursor-position*) x
                               (gamekit:y *cursor-position*) y)))

  (gamekit:bind-button :mouse-left :pressed
                       (lambda ()
                         (unless *cursor-pressed*
                           (setf *cursor-pressed* t)
                           (let ((v (vertex (gamekit:x *cursor-position*)
                                            (gamekit:y *cursor-position*))))
                             (cond ((null *polygon*)
                                    (setf *polygon* (polygon v)))
                                   (t
                                    (insert-prev! v (polygon-head *polygon*))
                                    (set-ears! *polygon*)))))))

  (gamekit:bind-button :mouse-left :released
                       (lambda () (setf *cursor-pressed* nil)))

  (gamekit:bind-button :k :pressed
                       (lambda () (setf *polygon* nil)))

  (gamekit:bind-button :e :pressed
                       (lambda () (setf *render-mode* ':EARS)))

  (gamekit:bind-button :d :pressed
                       (lambda () (setf *render-mode* ':DEFAULT)))

  (gamekit:bind-button :t :pressed
                       (lambda () (setf *render-mode* ':TRIANGULATION))))

(defvar *black* (gamekit:vec4 0 0 0 1))
(defvar *line-thickness* 5.0)

(defun vertex->vec2 (v)
  (gamekit:vec2 (x v) (y v)))

(defun draw-ear (nv)
  (gamekit:draw-line (vertex->vec2 (node-vertex (node-prev nv)))
                     (vertex->vec2 (node-vertex (node-next nv)))
                     *black*))

(defmethod gamekit:draw ((app flatso-demo))
  (let ((vertices (mapcar #'vertex->vec2
                          (vertex-list *polygon*))))
    (cond ((endp vertices) nil)
          ((endp (rest vertices))
           (gamekit:draw-circle (first vertices)
                                (/ *line-thickness* 2)
                                :fill-paint *black*))
          (t
           (gamekit:draw-polygon vertices
                                 :stroke-paint *black*
                                 :thickness *line-thickness*)
           (case *render-mode*
             (:DEFAULT nil)
             (:EARS
              (dopolygon (nv *polygon*)
                (when (node-ear-p nv)
                  (draw-ear nv))))
             (:TRIANGULATION
              (when (simple-p *polygon*)
                (dolist (edge (triangulation *polygon*))
                  (gamekit:draw-line (vertex->vec2 (car edge))
                                     (vertex->vec2 (cdr edge))
                                     *black*)))))))))


(setf *polygon* (polygon (vertex 100 100) (vertex 615 86) (vertex 575 220) (vertex 460 316) (vertex 350 380)))
