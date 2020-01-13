(setf *print-circle* t)

(defstruct (vertex (:constructor vertex (x y))
                   (:conc-name nil))
  x
  y)

(defun vertex= (a b)
  (check-type a vertex)
  (check-type b vertex)
  (and (= (x a) (x b))
       (= (y a) (y b))))

(defstruct (node (:constructor %make-node))
  "An element in a double-linked circular VERTEX list."
  vertex
  (ear-p nil)
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


(defmacro dopolygon ((var polygon-form &optional result-form)
                     &body body)
  "Iterate over the nodes of a polygon. Like DOLIST."
  (check-type var symbol)
  (let ((node (gensym "node"))
        (poly-head (gensym "node")))
    `(let ((,poly-head (polygon-head ,polygon-form))
           (,var nil))
       (loop :for ,node := ,poly-head :then (node-next ,node)
             :do (progn
                   (setf ,var ,node)
                   ,@body)
             :until (eq (node-next ,node) ,poly-head))
       ,result-form)))


(defun vertex-list (polygon)
  "Get the list of VERTICES associated with POLYGON, traversed in counter-clockwise order."
  (if polygon
      (let ((verts nil))
        (dopolygon (nv polygon (nreverse verts))
          (push (node-vertex nv) verts)))
      nil))

(defun num-vertices (polygon)
  "The number of vertices in POLYGON."
  (let ((count 0))
    (dopolygon (nv polygon count)
      (incf count))))

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

(defun nth-node (n polygon)
  "Get the Nth node of POLYGON."
  (check-type n integer)
  (let ((step-fn (if (plusp n) #'node-next #'node-prev)))
    (let ((nv (polygon-head polygon)))
      (dotimes (i (abs n) nv)
        (setf nv (funcall step-fn nv))))))

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

;;; TODO: write DOVERTICES, DOEDGE instead of DOPOLYGON

;;; TODO: this fails on some wacky degenerate cases. also, we check more edges than necessary
(defun simple-p (polygon)
  (dopolygon (nv polygon t)
    ;; basic idea: each edge in a simple polygon intersects 2 others.
    (let ((hit-count 0))
      (dopolygon (ne polygon)
        (when (intersects-p (node-vertex nv) (node-vertex (node-next nv))
                            (node-vertex ne) (node-vertex (node-next ne)))
          (incf hit-count)
          (when (> hit-count 3)         ; 3 since the edge trivially intersects itself
            (return-from simple-p nil)))))))

;;; Triangulation Routines

;;; TODO: adopt convention that a,b,c vertices, na,nb,nc nodes

(defun in-cone-p (na b)
  "Given a node NA, determine whether the directed segment from A to B lies within the cone at NA."
  (check-type na node)
  (check-type b vertex)
  (let ((a- (node-vertex (node-prev na)))
        (a  (node-vertex na))
        (a+ (node-vertex (node-next na))))
    (if (left-on-p a a+ a-)
        ;; a-,a,a+ has angle <= pi radians
        (and (left-p a b a-)
             (left-p b a a+))
        ;; a-,a,a+ has angle > pi radians
        (not (and (left-on-p a b a+)
                  (left-on-p b a a-))))))

(defun diagonal-p (na nb)
  "Determine whether two nodes NA and NB form a diagonal segment AB of POLYGON."
  ;; NOTE: It is assumed that NA and NB are actually nodes of POYLYGON!
  (check-type na node)
  (check-type nb node)
  ;; Check: is the segment AB locally interior to POLYGON at A,B?
  (unless (and (in-cone-p na (node-vertex nb))
               (in-cone-p nb (node-vertex na)))
    (return-from diagonal-p nil))

  ;; Ok, walk edges and check for nontrivial intersections
  (dopolygon (nv (make-polygon :head na) t)
    (let ((nv+ (node-next nv)))
      (when (and (not (eq na nv)) (not (eq na nv+))
                 (not (eq nb nv)) (not (eq nb nv+))
                 (intersects-p (node-vertex na) (node-vertex nb)
                               (node-vertex nv) (node-vertex nv+)))
        (return-from diagonal-p nil)))))


(defun set-ears! (polygon)
  "Set the EAR-P flag on nodes in POLYGON to T if the node represents an ear.

Here 'ear' means that the node's immediate neighbors form a diagonal of the polygon."
  (dopolygon (nv polygon)
    (setf (node-ear-p nv) (diagonal-p (node-prev nv)
                                      (node-next nv)))))

(defun trim-ear! (nv)
  (let ((nv- (node-prev nv))
        (nv+ (node-next nv)))
    (setf (node-ear-p nv-) (diagonal-p (node-prev nv-) nv+))
    (setf (node-ear-p nv+) (diagonal-p nv- (node-next nv+)))
    (setf (node-next nv-) nv+)
    (setf (node-prev nv+) nv-)))

(defun triangulation (polygon)
  (let ((copy (apply #'polygon (vertex-list polygon))))
    (set-ears! copy)
    (let ((edges nil))
      (loop :for n :downfrom (num-vertices copy) :above 3
            :do
               (loop :for nv := (polygon-head copy) :then nv+
                     :for nv- := (node-prev nv)
                     :for nv+ := (node-next nv)
                     :when (node-ear-p nv)
                       :do (progn
                             (push (cons (node-vertex nv-) (node-vertex nv+)) edges)
                             (trim-ear! nv)
                             (setf (polygon-head copy) nv+) ; in case head is nv
                             )
                     :until (node-ear-p nv)))
      edges)))
