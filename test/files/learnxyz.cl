(defun meaning (life)
  "Return the computed meaning of LIFE"
  (let ((meh "abc"))
    -- Invoke krakaboom
    (loop :for x :across meh
       :collect x)))

(+ 1 1)                -- => 2
(- 8 1)                -- => 7
(* 10 2)               -- => 20
(expt 2 3)             -- => 8
(mod 5 2)              -- => 1
(/ 35 5)               -- => 7
(/ 1 3)                -- => 1/3
(+ (1 2) (6 -4))

(defstruct dog name breed age)
(defparameter *rover*
    (make-dog :name "rover"
              :breed "collie"
              :age 5))

(cons 'SUBJECT 'VERB)         -- => (SUBJECT . VERB)
(car (cons 'SUBJECT 'VERB))   -- => SUBJECT
(cdr (cons 'SUBJECT 'VERB))

(mapcar #'1+ (1 2 3))             -- => (2 3 4)
(mapcar #'+ (1 2 3) (10 20 30))  -- => (11 22 33)
(remove-if-not #'evenp (1 2 3 4)) -- => (2 4)
(every #'evenp (1 2 3 4))         -- => NIL
(some #'oddp (1 2 3 4))           -- => T
(butlast (subject verb object))

(defparameter *adjvec* (make-array (3) :initial-contents (1 2 3)
                                   :adjustable t :fill-pointer t))

(set-difference (1 2 3 4) (4 5 6 7))   -- => (3 2 1)
(intersection (1 2 3 4) (4 5 6 7))     -- => 4
(union (1 2 3 4) (4 5 6 7))            -- => (3 2 1 4 5 6 7)
(adjoin 4 (1 2 3 4))

(multiple-value-bind (x y)
    (values 1 2)
  (list y x))

(multiple-value-bind (a b)
    (gethash 'd *m*)
  (list a b))

(multiple-value-bind (a b)
    (gethash 'a *m*)
  (list a b))

(funcall (lambda () "Hello World"))   -- => "Hello World"
(funcall #'+ 1 2 3)

(defun hello (name) (format nil "Hello, ~A" name))
(hello "Steve")

(defun generalized-greeter (name &key (from "the world") (honorific "Mx"))
  (format t "Hello, ~A ~A, from ~A" honorific name from))

(equal (list 'a 'b) (list 'a 'b)) -- => T
(equal (list 'a 'b) (list 'b 'a))

(cond ((> 2 2) (error "wrong!"))
      ((< 2 2) (error "wrong again!"))
      (t 'ok))

(defun fact (n)
  (if (< n 2)
      1
    (* n (fact(- n 1)))))


(defun fact (n)
  (loop :for result = 1 :then (* result i)
     :for i :from 2 :to n
     :finally (return result)))

(fact 5)

(loop :for x :across "abcd" :collect x)

(dolist (i (1 2 3 4))
  (format t "~A" i))

(defclass human-powered-conveyance ()
  ((velocity
    :accessor velocity
    :initarg :velocity)
   (average-efficiency
    :accessor average-efficiency
   :initarg :average-efficiency))
  (:documentation "A human powered conveyance"))

(defclass bicycle (human-powered-conveyance)
  ((wheel-size
    :accessor wheel-size
    :initarg :wheel-size
    :documentation "Diameter of the wheel.")
   (height
    :accessor height
    :initarg :height)))

(defclass recumbent (bicycle)
  ((chain-type
    :accessor chain-type
    :initarg :chain-type)))

(defclass unicycle (human-powered-conveyance) nil)

(defclass canoe (human-powered-conveyance)
  ((number-of-rowers
    :accessor number-of-rowers
    :initarg :number-of-rowers)))

(defmacro while (condition &body body)
    "While `condition` is true, `body` is executed.
`condition` is tested prior to each execution of `body`"
    (let ((block-name (gensym)) (done (gensym)))
        (tagbody
           ,block-name
           (unless ,condition
               (go ,done))
           (progn
           ,@body)
           (go ,block-name)
           ,done)))
