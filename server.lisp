(load "~/quicklisp/setup.lisp")
(ql:quickload :trivia)
(ql:quickload "jsown")
(ql:quickload :bordeaux-threads)
(ql:quickload :hunchensocket)
(ql:quickload :alexandria)

(defconstant n-entities 10)

(defmacro case* (x p &rest body)
  (let ((cases nil))
    (dolist (raw-case body)
      (trivia:match raw-case
        ((list* s foo nil)
         (if (consp s)
             (dolist (item s)
               (setf cases (cons (list (list 'funcall p x item) foo) cases)))
             (setf cases (cons (list (list 'funcall p x s) foo) cases))))))
    `(cond
       ,@cases)))


(defclass ecs ()
  ((free-ids :initform (alexandria:iota n-entities)
             :reader free-ids)

   (point-mass :initform (make-array n-entities :initial-element nil)
               :reader point-masses)

   (health :initform (make-array n-entities :initial-element nil)
           :reader health)))

(defmethod jsown:to-json ((e ecs))
  (let ((point-masses-for-jsown
         (loop
            for component being the elements of (slot-value e 'point-mass)
            for entity-id from 0
            when component
            collect (vector entity-id component))))
    (jsown:to-json
     (jsown:new-js ("point-masses" point-masses-for-jsown)))))


(defstruct point-mass
  x y
  vel-x vel-y)

(defmethod jsown:to-json ((pm point-mass))
  (jsown:to-json
   (jsown:new-js ("x" (point-mass-x pm))
                 ("y" (point-mass-y pm))
                 ("vel-x" (point-mass-vel-x pm))
                 ("vel-y" (point-mass-vel-y pm)))))


(defun ecs-add-entity (ecs &key point-mass health)
  "Adds an entity to ECS with the given components and returns the id assigned to it."
;;(declare (type ecs ecs))
  (let ((entity-id (pop (slot-value ecs 'free-ids))))

    (cond
      ((null entity-id) (error "no entity ids left"))
      (t
        
       (if point-mass
           (setf (svref (slot-value ecs 'point-mass) entity-id)
                 point-mass))

       (if health
           (setf (svref (slot-value ecs 'health) entity-id)
                 health))

       ;; return the entity's id
       entity-id))))
      
   
(defun ecs-remove-entity (ecs entity-id)
  "Removes the entity with the given ENTITY-ID."
  (declare (type ecs ecs))
  (declare (type fixnum entity-id))

  (if (not (<= 0 entity-id n-entities))
      (error (format nil "entity ids must be between 0 and ~a, got ~a"
                     n-entities entity-id))

      (if (member entity-id (slot-value ecs 'free-ids))
          (error (format nil "there is no entity with id ~a"
                         entity-id))

          (progn
            ;; set components to nil
            (setf (svref (slot-value ecs 'point-mass) entity-id) nil)
            (setf (svref (slot-value ecs 'health) entity-id) nil)

            ;; mark the entity id as free so it can be used again
            (push entity-id (slot-value ecs 'free-ids))))))
             
      

(defclass chat-room (hunchensocket:websocket-resource)
  ((name
    :initarg :name
    :initform (error "name this channel")
    :reader name))
  (:default-initargs :client-class 'user))

(defvar *users-lock* (bt:make-lock "users-lock"))

(defclass user (hunchensocket:websocket-client)
  ((name :initarg :user-agent
         :reader name
         :initform "anon")
   (x :initarg :x
      :reader x
      :initform 0.0)
   (y :initarg :y
      :reader y
      :initform 0.0)
   (vel-x :initarg :vel-x
          :reader vel-x
          :initform 0)
   (vel-y :initarg :vel-y
          :reader vel-y
          :initform 0)
   (entity-id :initarg :entity-id
              :reader entity-id
              :initform nil)))


(defmethod jsown:to-json ((u user))
  "Returns a jsown object that represents the given user."
  (jsown:to-json
   (jsown:new-js ("name" (slot-value u 'name))
                 ("vel-x" (slot-value u 'vel-x))
                 ("vel-y" (slot-value u 'vel-y))
                 ("x" (slot-value u 'x))
                 ("y" (slot-value u 'y)))))
  

(defvar *chat-rooms* (list (make-instance 'chat-room :name "/game")))

(defvar *ecs* (make-instance 'ecs))

(defun find-room (request)
  (find (hunchentoot:script-name request) *chat-rooms* :test #'string= :key #'name))

(pushnew 'find-room hunchensocket:*websocket-dispatch-table*)

(defun broadcast (room message &rest args)
  (loop for peer in (hunchensocket:clients room)
        do (hunchensocket:send-text-message peer (apply #'format nil message args))))

(defmethod hunchensocket:client-connected ((room chat-room) user)
  (bt:with-lock-held (*users-lock*)
    (let ((new-entity-id (ecs-add-entity *ecs*
                                         :point-mass (make-point-mass :x 0.0 :y 0.0 :vel-x 0 :vel-y 0)
                                         :health "health")))
      (setf (slot-value user 'entity-id) new-entity-id))

    (format t "resource now has ~a clients~%"
            (length (hunchensocket:clients room)))
    (format t "~{~a~%~}" (hunchensocket:clients room))
    (let ((msg (jsown:to-json (jsown:new-js ("msg-type" "hello")))))
      (broadcast room msg (name user) (name room)))))

(defmethod hunchensocket:client-disconnected ((room chat-room) user)
  (bt:with-lock-held (*users-lock*)
    (ecs-remove-entity *ecs* (slot-value user 'entity-id))
    (broadcast room "~a has left ~a" (name user) (name room))))

(defmethod hunchensocket:text-message-received ((room chat-room) user raw-msg)
  (bt:with-lock-held (*users-lock*)
    (let ((msg (jsown:parse raw-msg)))
      (format t "msg from client: ~a~%" msg)
      (case* (jsown:val msg "msg-type") #'equalp
             ("set-vel-x"
              (progn
                (setf (slot-value user 'vel-x)
                      (jsown:val msg "vel-x"))
                (format t "slot-value vel-x: ~a (~a)~%"
                        (slot-value user 'vel-x)
                        (jsown:val msg "vel-x"))))
              
             ("set-vel-y"
              (setf (slot-value user 'vel-y)
                    (jsown:val msg "vel-y")))))))



(defun ecs-movement (ecs tick)
  (declare (type ecs ecs))
  (declare (type float tick))
  (assert (< 0.0 tick))
  (loop
     for component being the elements of (slot-value ecs 'point-mass)
     for entity-id from 0
     when (not (null component))
     do (format t "point mass component of entity ~a: ~a~%"
                entity-id component)

       (setf (point-mass-x component)
             (+ (point-mass-x component)
                (* tick (point-mass-vel-x component))))

       (setf (point-mass-y component)
             (+ (point-mass-y component)
                (* tick (point-mass-vel-y component))))))

  

(defun game-loop ()
  (loop
     do (let ((tick 0.25))
          (sleep tick)
          (bt:with-lock-held (*users-lock*)
            (ecs-movement *ecs* tick)
            (let ((msg (jsown:to-json (jsown:new-js ("msg-type" "players")
                                                    ("ecs" *ecs*)))))
              (format t "~a~%" msg)
              (broadcast (car *chat-rooms*) msg))))))


(hunchentoot:start (make-instance 'hunchensocket:websocket-acceptor :port 5000))
(bt:join-thread (bt:make-thread #'game-loop))
