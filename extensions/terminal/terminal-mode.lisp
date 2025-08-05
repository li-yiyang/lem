;;;; terminal-mode.lisp --- Terminal mode for Lem on 3bst

;;; Commentary:
;; This file is based on original lem-terminal (#0bfb210a), but
;; replaced the terminal implementation with 3bst.
;;
;; The overall design is described as below:
;;
;;                          toggle
;;                        copy-mode-p
;;        terminal-mode <-------------> terminal-copy-mode
;;  
;;                   / term:   3bst:term          => used to processing rendering
;;         terminal +- proc:   uiop:process-info  => used to hold the terminal process
;;                   \ thread: bt:thread          => used to render terminal to buffer
;;
;;    Methods:
;;    + (destroy buffer): terminate proc, close thread
;;    + (attach-terminal  buffer): attach buffer with terminal
;;    + (buffer-terminal-alive-p buffer): test if buffer has a living terminal
;;    + (buffer-terminal buffer): get the terminal of the buffer
;;
;;    Commands:
;;    + terminal-input: send the key sequence to terminal
;;    + terminal-key-{...}: send the key press to terminal
;; 
;;  copy-mode-p                           => t or nil
;;                                           if copy-mode-p is true, not update buffer terminal rendering 

(defpackage #:lem-terminal/terminal-mode
  (:use :cl :lem)
  (:import-from :alexandria
   #:when-let)
  (:export 
   #:*shell*
   #:*new-terminal-behavior*
   #:*terminal-buffer-name*
   #:terminal
   #:terminal-resize
   #:buffer-terminal
   #:terminal-proc))

(in-package :lem-terminal/terminal-mode)

;;; Configurations

;; see `attach-terminal' for implementation
(declaim (type (or string list) *shell*))
(defparameter *shell*
  ;; DEV: for test only
  '("ls" "-la")
  
  ;; TODO: 
  ;; (or (uiop:getenv "SHELL")
  ;;     "bash")
  "The shell of lem terminal. 
It should be a string or a list of string to create the terminal shell. 

Example: 

    (setf lem-terminal/terminal-mode:*shell* \"zsh\")
    (setf lem-terminal/terminal-mode:*shell* '(\"screen\" \"bash\"))

By default, the `*shell*' should be the $SHELL env variable,
or \"bash\". ")

;; see command `terminal' for implementation
(declaim (type (member :new :reuse) *new-terminal-behavior*))
(defparameter *new-terminal-behavior* :reuse
  "How the command `terminal' creates the terminal. 

It should be one of: 
+ `:new': create a new buffer with unique name \"*Terminal*\"
+ `:reuse': reuse existing terminal, aka, switch to 
  existing buffer named \"*Terminal*\" and ensure it's
  attached with a living terminal. ")

(declaim (type string *terminal-buffer-name*))
(defparameter *terminal-buffer-name* "*Terminal*"
  "Default terminal buffer name. 

The command `terminal' creates terminal buffer with
`*terminal-buffer-name*' as buffer name. ")

;;; buffer-terminal
;; A structure with value:
;; + term
;; + proc
;; + thread

(defstruct terminal
  term
  proc
  thread)

;;; Buffer Attributes

(defun buffer-terminal (buffer)
  "Return the `terminal' object of BUFFER or nil. "
  (declare (type buffer buffer))
  (buffer-value buffer 'buffer-terminal))

(defun buffer-terminal-alive-p (buffer)
  "Return t if terminal of BUFFER is alive. "
  (declare (type buffer buffer))
  (the boolean
       (when-let ((term (buffer-terminal buffer)))
         (uiop:process-alive-p (terminal-proc term)))))

;;; Render Terminal to Buffer

;; + color<-3bst-color
;; + attr<-glyph
;; + move-to-3bst-term
;; are used as a wrapper of 3bst

(let ((cache (make-hash-table)))
  (defun color<-3bst-color (3bst-color)
    "Turn 3BST-COLOR into Lem color. 
The color is cached for performance. "
    (declare (type unsigned-byte 3bst-color))
    (the color
         (or (gethash 3bst-color cache)
             ;; TODO: make terminal color match with theme color
             ;; or should it be done in `attr<-glyph' function? 
             (destructuring-bind (r g b) (3bst::color-rgb 3bst-color)
               (make-color r g b)))))
  
  (defun clear-terminal-color-cache ()
    "Clear `color<-3bst-color' cache. 
Should be called after `load-theme' command. "
    (clrhash cache)))

(defun attr<-glyph (glyph)
  "Turn 3bst::glyph into Lem attribute. 
Return Lem attribute. "
  (declare (type 3bst::glyph glyph))
  (let ((mode (3bst::mode glyph)))
    (make-attribute :foreground (color<-3bst-color (3bst::fg glyph))
                    :background (color<-3bst-color (3bst::bg glyph))
                    :reverse    (logtest 3bst::+attr-reverse+   mode)
                    :bold       (logtest 3bst::+attr-bold+      mode)
                    :underline  (logtest 3bst::+attr-underline+ mode))))

(defun move-to-3bst-term (point 3bst-term)
  "Move POINT to the 3BST-TERM position. "
  (declare (type point point)
           (type 3bst:term 3bst-term))
  (let ((cursor (3bst::cursor 3bst-term)))
    (move-to-line   point (1+ (3bst::y cursor)))
    (move-to-column point (1+ (3bst::x cursor)))))

;; + terminal-resize
;; + terminal-render
;; these are use to control term and buffer rendering

(defun term-resize (terminal window)
  "Resize TERMINAL to fit WINDOW. "
  (declare (type terminal terminal)
           (type window   window))
  (let ((cols (window-width window))
        (rows (1- (window-height window)))
        (term (terminal-term terminal)))
    ;; resize only when TERMINAL size is different to WINDOW size
    (unless (and (= cols (3bst::columns term))
                 (= rows (3bst::rows    term)))
      (3bst::tresize cols rows :term term))))

;; TODO: redraw the buffer line if the row is marked as dirty
(defun terminal-render (term buffer)
  "Render TERMINAL on BUFFER. "
  (declare (type 3bst:term term)
           (type buffer    buffer))
  (erase-buffer buffer)
  (loop :with rows  := (3bst:rows    term)
        :with cols  := (3bst:columns term)
        :with point := (buffer-point buffer)
        
        :for row :from 0 :below rows
        :do (progn
              (loop :with prev-attr := nil
                    :with string    := ()

                    :for col :from 0 :below cols
                    :for glyph := (3bst:glyph-at (3bst::screen term) row col)
                    :for chr   := (3bst::c glyph)
                    :for attr  := (setf prev-attr (attr<-glyph glyph))
                    :then (attr<-glyph glyph)

                    :do (progn 
                          (push chr string)
                          (unless (attribute-equal prev-attr attr)
                            (insert-string point
                                           (concatenate 'string (nreverse string))
                                           :attribute prev-attr)
                            (setf prev-attr attr 
                                  string    ())))
                    
                    :finally (insert-string point
                                            (concatenate 'string (nreverse string))
                                            :attribute prev-attr))
              (insert-character point #\Newline))

        ;; update point position
        :finally (move-to-3bst-term point term)))

;;; Attach Terminal

(defun attach-terminal (buffer &key (directory (or 
                                                (buffer-directory (current-buffer))
                                                (uiop:getcwd)))
                                    (cols      80)
                                    (rows      40))
  "Attach BUFFER with a `terminal' value if buffer-terminal is dead. 

If BUFFER terminal is dead ;; (buffer-terminal-alive-p buffer) => nil
a new terminal would be created and binded with BUFFER.

Otherwise, the BUFFER would remain as it is.

Parameters:
+ BUFFER: the buffer terminal should to be attached
+ DIRECTORY: the directory that `*shell*' process to be lanuched
+ COLS and ROWS: size of created terminal
"
  (declare (type buffer buffer))
  (unless (buffer-terminal-alive-p buffer)
    ;; the BUFFER should be cleared
    (erase-buffer buffer)
    ;; and ROWS of empty lines should be inserted
    ;; see `terminal-render' and `3bst:dirty'
    (dotimes (i rows) (newline))
    (let* ((term (make-instance '3bst:term
                                :columns cols
                                :rows    rows))
           (proc (uiop:with-current-directory (directory)
                   (uiop:launch-program *shell*
                                        :input           :stream
                                        :output          :stream
                                        :external-format :utf-8)))
           (out    (uiop:process-info-output proc))
           (thread (bt:make-thread 
                    (lambda ()
                      (loop :for chr = (read-char out nil :eof)
                            :until (eq chr :eof)
                            :do (3bst:handle-input (string chr) :term term))))))
      (setf (buffer-value buffer 'buffer-terminal)
            (make-terminal :term term :proc proc :thread thread)))))

;;; DEV Note:

;; the following test code would render ls command output to terminal buffer
;;

;; (let* ((*shell* '("htop"))
;;        (buff    (or (get-buffer  *terminal-buffer-name*)
;;                     (make-buffer *terminal-buffer-name*)))
;;        (win     (setf (current-window) (pop-to-buffer buff))))
;;   (attach-terminal buff 
;;                    :cols (1- (window-width win))
;;                    :rows (1- (window-height win))
;;                    :directory #P"~/")
;;   (bt:make-thread
;;    (lambda ()
;;      (loop :while (uiop:process-alive-p (terminal-proc (buffer-terminal buff)))
;;            :do (send-event 
;;                 (lambda () 
;;                   (terminal-render (terminal-term (buffer-terminal buff)) buff)
;;                   (redraw-display)))
;;            :do (sleep 1.0)
;;            :finally (uiop:terminate-process (terminal-proc (buffer-terminal buff)))))))

;;; TODO: Commands

;;; Hooks

;; (add-hook *after-load-theme-hook* 'clear-terminal-color-cache)

;;;; terminal-mode.lisp ends here