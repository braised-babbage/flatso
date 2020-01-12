(defvar *canvas-width* 800)
(defvar *canvas-height* 600)
(defvar *cursor-position* (gamekit:vec2 0 0))
(defvar *cursor-pressed* nil)

(defparameter *polygon* (polygon (vertex 100 100) (vertex 200 100) (vertex 100 200)))

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
                                    (insert-prev! v (polygon-head *polygon*))))))))

  (gamekit:bind-button :mouse-left :released
                       (lambda ()
                         (setf *cursor-pressed* nil)))

  (gamekit:bind-button :k :pressed
                       (lambda ()
                         (setf *polygon* nil))))

(defvar *black* (gamekit:vec4 0 0 0 1))
(defvar *line-thickness* 5.0)

(defmethod gamekit:draw ((app flatso-demo))
  (let ((vertices (mapcar (lambda (v)
                            (gamekit:vec2 (x v) (y v)))
                          (vertex-list *polygon*))))
    (cond ((endp vertices) nil)
          ((endp (rest vertices))
           (gamekit:draw-circle (first vertices)
                                (/ *line-thickness* 2)
                                :fill-paint *black*))
          (t
           (gamekit:draw-polygon vertices
                                 :stroke-paint *black*
                                 :thickness *line-thickness*)))))
