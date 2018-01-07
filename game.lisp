(load "~/quicklisp/setup.lisp")
(ql:quickload "sdl2")
(ql:quickload "jsown")
(ql:quickload "trivia")
(ql:quickload :websocket-driver-client)

(require :sdl2)
(require :cl-opengl)

(defconstant ws-server "ws://localhost:5000/game")

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




(defstruct sprite
  color
  vertices)
  
(defstruct player
  sprite
  x y
  vel-x vel-y)
  

(defconstant plane-sprite
  (make-sprite :color (vector 1. 0. 0.)
               :vertices (vector (vector 0.0 0.3)
                                 (vector -0.3 -0.3)
                                 (vector 0.3 -0.3))))

(defmacro dolines (hd &rest body)
  (trivia:match hd
    ((list* path elt nil)
     `(with-open-file (str ,path :direction :input)
        (do ((,elt (read-line str nil :eof)
                   (read-line str nil :eof)))

            ((eq :eof ,elt) 'done)

        ,@body)))))



(defun render-sprite (s x y)
  (declare (type sprite s))
  (declare (type number x))
  (declare (type number y))
  (gl:begin :triangles)
  (let ((c (sprite-color s))
        (vertices (sprite-vertices s)))

    (gl:color (svref c 0)
              (svref c 1)
              (svref c 2))

     (loop for i from 0 to (1- (length vertices)) do

          (let ((vertex (svref vertices i)))

               (gl:vertex (+ x (svref vertex 0))
                          (+ y (svref vertex 1))))))
  (gl:end))
  

(defun run ()
  "The kitchen sink."
  (sdl2:with-init (:everything)
    (format t "Using SDL Library Version: ~D.~D.~D~%"
            sdl2-ffi:+sdl-major-version+
            sdl2-ffi:+sdl-minor-version+
            sdl2-ffi:+sdl-patchlevel+)
    (finish-output)

    (sdl2:with-window (win :flags '(:shown :opengl))
      (sdl2:with-gl-context (gl-context win)
        (let ((controllers ())
              (haptic ())
              (pos-x 0.0)
              (pos-y 0.0)
              (vel-x 0)
              (vel-y 0)
              (left-pressed nil)
              (right-pressed nil)
              (up-pressed nil)
              (down-pressed nil)
              (client (wsd:make-client ws-server))
              (players nil)
              (last-update (get-internal-real-time)))

          (wsd:start-connection client)
          (wsd:on :message client
                  (lambda (raw-msg)
                    (let ((msg (jsown:parse raw-msg)))
                      (if (equalp "players" (jsown:val msg "msg-type"))
                          (progn
                            (setf players nil)
                            (dolist (p (jsown:val msg "players"))
                              (push (make-player :x (jsown:val p "x")
                                                 :y (jsown:val p "y")
                                                 :vel-x (jsown:val p "vel-x")
                                                 :vel-y (jsown:val p "vel-y"))
                                    players))
                            (setf last-update (get-internal-real-time)))))))
                      
          ;; basic window/gl setup
          (format t "Setting up window/gl.~%")
          (finish-output)
          (sdl2:gl-make-current win gl-context)
          (gl:viewport 0 0 800 600)
          (gl:matrix-mode :projection)
          (gl:ortho -2 2 -2 2 -2 2)
          (gl:matrix-mode :modelview)
          (gl:load-identity)
          (gl:clear-color 0.0 0.0 0.0 1.0)
          (gl:clear :color-buffer)

          ;; main loop
          (format t "Beginning main loop.~%")
          (finish-output)
          (sdl2:with-event-loop (:method :poll)
            (:keydown (:keysym keysym)
              (case* (sdl2:scancode-value keysym) #'sdl2:scancode=
                ((:scancode-w :scancode-up) (setf up-pressed t))
                ((:scancode-s :scancode-down) (setf down-pressed t))
                ((:scancode-a :scancode-left) (setf left-pressed t))
                ((:scancode-d :scancode-right) (setf right-pressed t))
                (:scancode-space (format t "shoot~%")))
              
              (let ((new-vel-y (+ (if up-pressed 1 0)
                                  (if down-pressed -1 0))))
                (cond
                  ((/= new-vel-y vel-y)
                   (setf vel-y new-vel-y)
                   (wsd:send client (jsown:to-json (jsown:new-js ("msg-type" "set-vel-y") ("vel-y" vel-y)))))))

              (let ((new-vel-x (+ (if left-pressed -1 0)
                                  (if right-pressed 1 0))))
                (cond
                  ((/= new-vel-x vel-x)
                   (setf vel-x new-vel-x)
                   (wsd:send client (jsown:to-json (jsown:new-js ("msg-type" "set-vel-x") ("vel-x" vel-x))))))))

            (:keyup
              (:keysym keysym)
              (case* (sdl2:scancode-value keysym) #'sdl2:scancode=
                (:scancode-escape (sdl2:push-event :quit))
                ((:scancode-w :scancode-up) (setf up-pressed nil))
                ((:scancode-s :scancode-down) (setf down-pressed nil))
                ((:scancode-a :scancode-left) (setf left-pressed nil))
                ((:scancode-d :scancode-right) (setf right-pressed nil)))

              (let ((new-vel-y (+ (if up-pressed 1 0)
                                  (if down-pressed -1 0))))
                (cond
                  ((/= new-vel-y vel-y)
                   (setf vel-y new-vel-y)
                   (wsd:send client (jsown:to-json (jsown:new-js ("msg-type" "set-vel-y") ("vel-y" vel-y)))))))

              (let ((new-vel-x (+ (if left-pressed -1 0)
                                  (if right-pressed 1 0))))
                (cond
                  ((/= new-vel-x vel-x)
                   (setf vel-x new-vel-x)
                   (wsd:send client (jsown:to-json (jsown:new-js ("msg-type" "set-vel-x") ("vel-x" "vel-x" vel-x))))))))

            (:idle
             ()
             (gl:clear :color-buffer)
             (dolist (p players)
               (let* ((time-delta-s (/ (- (get-internal-real-time)
                                          last-update)
                                       (float internal-time-units-per-second)))

                      (new-x (+ (player-x p) (* time-delta-s (player-vel-x p))))
                      (new-y (+ (player-y p) (* time-delta-s (player-vel-y p)))))

                 (setf (player-x p) new-x)
                 (setf (player-y p) new-y)))
             
             (setf last-update (get-internal-real-time))

             (dolist (p players)
               (render-sprite plane-sprite (player-x p) (player-y p)))

             (gl:flush)
             (sdl2:gl-swap-window win))

            (:quit () t))

          (finish-output))))))



(run)



