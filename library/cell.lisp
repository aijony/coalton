(coalton-library/utils:defstdlib-package #:coalton-library/cell
  (:use
   #:coalton
   #:coalton-library/builtin
   #:coalton-library/classes
   #:coalton-library/list)
  (:export
   #:Cell
   #:new
   #:read
   #:swap!
   #:write!
   #:update!
   #:update-swap!
   #:push!
   #:pop!
   #:increment!
   #:decrement!))

#+coalton-release
(cl:declaim #.coalton-impl:*coalton-optimize-library*)

(cl:in-package #:coalton-library/cell)
 
(cl:declaim (cl:inline make-cell-internal))

(cl:defstruct cell-internal
  (inner (cl:error "") :type cl:t))

(cl:defmethod cl:print-object ((self cell-internal) stream)
  (cl:format stream "#.(CELL ~A)" (cell-internal-inner self))
  self)

#+sbcl
(cl:declaim (sb-ext:freeze-type cell-internal))

(coalton-toplevel

  (repr :transparent)
  (define-type (Cell :a)
    "Internally mutable cell"
    (%Cell Lisp-Object))

  (declare new (:a -> Cell :a))
  (define (new data)
    "Create a new mutable cell"
    (lisp (Cell :a) (data)
      (make-cell-internal :inner data)))

  (declare read (Cell :a -> :a))
  (define (read cel)
    "Read the value of a mutable cell"
    (lisp :a (cel)
      (cell-internal-inner cel)))

  (declare swap! (Cell :a -> :a -> :a))
  (define (swap! cel data)
    "Replace the value of a mutable cell with a new value, then return the old value"
    (lisp :a (data cel)
      (cl:let* ((old (cell-internal-inner cel)))
        (cl:setf (cell-internal-inner cel) data)
        old)))

  (declare write! (Cell :a -> :a -> :a))
  (define (write! cel data)
    "Set the value of a mutable cell, returning the new value"
    (lisp :a (data cel)
      (cl:setf (cell-internal-inner cel) data)))

  (declare update! ((:a -> :a) -> Cell :a -> :a))
  (define (update! f cel)
    "Apply F to the contents of CEL, storing and returning the result"
    (write! cel (f (read cel))))

  (declare update-swap! ((:a -> :a) -> Cell :a -> :a))
  (define (update-swap! f cel)
    "Apply F to the contents of CEL, swapping the result for the old value"
    (swap! cel (f (read cel))))

;;; operators on cells of lists
  (declare push! (Cell (List :elt) -> :elt -> List :elt))
  (define (push! cel new-elt)
    (update! (Cons new-elt) cel))

  (declare pop! (Cell (List :elt) -> Optional :elt))
  (define (pop! cel)
    (match (read cel)
      ((Cons fst rst)
       (progn (write! cel rst)
              (Some fst)))
      ((Nil) None)))

;;; operators on cells of numbers
  (declare increment! (Num :counter => Cell :counter -> :counter))
  (define (increment! cel)
    "Add one to the contents of CEL, storing and returning the new value"
    (update! (+ 1) cel))

  (declare decrement! (Num :counter => (Cell :counter) -> :counter))
  (define (decrement! cel)
    "Add one to the contents of CEL, storing and returning the new value"
    (update! (- 1) cel))

  ;; i am very skeptical of these instances
  (define-instance (Eq :a => Eq (Cell :a))
    (define (== c1 c2)
      (== (read c1) (read c2))))

  (define-instance (Num :a => Num (Cell :a))
    (define (+ c1 c2)
      (new (+ (read c1) (read c2))))
    (define (- c1 c2)
      (new (- (read c1) (read c2))))
    (define (* c1 c2)
      (new (* (read c1) (read c2))))
    (define (fromInt i)
      (new (fromInt i))))

  (define-instance (Semigroup :a => Semigroup (Cell :a))
    (define (<> a b)
      (new (<> (read a) (read b)))))

  (define-instance (Functor Cell)
    (define (map f c)
      (new (f (read c)))))

  (define-instance (Applicative Cell)
    (define pure new)
    (define (liftA2 f c1 c2)
      (new (f (read c1) (read c2)))))

  (define-instance (Into :a (Cell :a))
    (define into new))

  (define-instance (Into (Cell :a) :a)
    (define into read)))

#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/CELL")
