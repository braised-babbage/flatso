(setf *print-circle* t)

(defstruct (vertex (:constructor vertex (x y))
                   (:conc-name nil))
  x
  y)

(defstruct (node (:constructor %make-node))
  "An element in a double-linked circular VERTEX list."
  vertex
  (next nil)
  (prev nil))

(defun make-node (vertex)
  "Construct a single, self-linked NODE from VERTEX."
  (let ((node (%make-node :vertex vertex)))
    (setf (node-next node) node
          (node-prev node) node)
    node))

(defun insert-prev! (vertex node)
  "Add VERTEX as the predecessor to NODE.

This destructively updates NODE's links."
  (let ((new (%make-node :vertex vertex
                         :next node
                         :prev (node-prev node)))
        (tail (node-prev node)))
    (setf (node-next tail) new)
    (setf (node-prev node) new)
    node))

(defstruct polygon
  "A representation of a polygon as a doubly-linked circular vertex list."
  head)

(defun polygon (vertex &rest vertices)
  "Construct a polygon from the given VERTICES."
  (let ((head (make-node vertex)))
    (dolist (v vertices)
      (insert-prev! v head))
    (make-polygon :head head)))

(defun vertex-list (polygon)
  "Get the list of VERTICES associated with POLYGON, traversed in counter-clockwise order."
  (if polygon
      (loop :for v := (polygon-head polygon) :then (node-next v)
            :collect (node-vertex v)
            :until (eq (node-next v) (polygon-head polygon)))
      nil))

(defun num-vertices (polygon)
  "The number of vertices in POLYGON."
  (loop :for v := (polygon-head polygon) :then (node-next v)
        :count :it
        :until (eq (polygon-head polygon) (node-next v))))

(defun area2 (a b c)
  "Compute twice the (signed) area of the triangle with vertices A,B,C."
  (- (* (- (x b) (x a))
        (- (y c) (y a)))
     (* (- (x c) (x a))
        (- (y b) (y a)))))

(defun polygon-area2 (polygon)
  "Compute twice the (signed) area of POLYGON."
  (loop :for v := (polygon-head polygon) :then nv
        :for nv := (node-next v)
        :for nnv := (node-next nv)
        :sum (area2 (node-vertex v)
                    (node-vertex nv)
                    (node-vertex nnv))
        :until (eq (polygon-head polygon) (node-next nnv))))


;;; Segment Intersexction

(defun collinear-p (a b c)
  "Is the vertex C on the line spanned by vertices A and B?"
  (zerop (area2 a b c)))

(defun left-p (a b c)
  "Is the vertex C to the left of the line spanned by vertices A and B?

This is with respect to the orientation induced by traversing AB from A to B."
  (plusp (area2 a b c)))

(defun left-on-p (a b c)
  "Is the vertex C to the left of, or on, the line spanned by vertices A and B?

This is with respect to the orientation induced by traversing AB from A to B."
  (>= (area2 a b c) 0))

(defun intersects-properly-p (a b c d)
  "Do the segments spanned by vertices A,B and C,D intersect in their interiors?"
  ;; We explicitly handle the degnerate case of collinearity (an 'improper'
  ;; intersection, at best).
  (cond ((collinear-p a b c) nil)
        ((collinear-p a b d) nil)
        ((collinear-p c d a) nil)
        ((collinear-p c d b) nil)
        (t
         ;; Now we simply check whether each pair of vertices is on opposite
         ;; sides of the other pair.
         (and (not (eq (left-p a b c) (left-p a b d)))
              (not (eq (left-p c d a) (left-p c d b)))))))

(defun between-p (a b c)
  "Is C contained in the segment spanned by A,B?"
  (cond ((not (collinear-p a b c)) nil)
        ((= (x a) (x b))
         (or (and (<= (y a) (y c)) (<= (y c) (y b)))
             (and (>= (y a) (y c)) (>= (y c) (y b)))))
        (t
         (or (and (<= (x a) (x c)) (<= (x c) (x b)))
             (and (>= (x a) (x c)) (>= (x c) (x b)))))))

(defun intersects-p (a b c d)
  "Do the segments spanned by vertices A,B and C,D intersect?"
  ;; The segments intersect iff they intersect propertly, or one of a segment's
  ;; vertices is on the other segment.
  (or (intersects-properly-p a b c d)
      (between-p a b c)
      (between-p a b d)
      (between-p c d a)
      (between-p c d b)))
