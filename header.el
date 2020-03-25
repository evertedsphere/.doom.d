;;; ~/.doom.d/header.el -*- lexical-binding: t; -*-

;; Fork of +light

(use-package! dired)
(use-package! libmpdel)

(defun +header-line--set-var-and-refresh-bars-fn (&optional symbol value)
  (when symbol
    (set-default symbol value))
  (when doom-init-time
    (+header-line-refresh-bars-h)))

;;
;;; Variables

(defcustom +header-line-height 47
  "The height of the header-line.

This is enforced by the xpm bitmap bar in `+header-line-bar'. Without it (and in
the terminal), this variable does nothing.

Use `setq!' to adjust this variable live, as it will trigger an refresh of the
bars in the header-line. `setq' will not."
  :type 'integer
  :set #'+header-line--set-var-and-refresh-bars-fn)

(defcustom +header-line-bar-width 3
  "The width of the bar in the header-line.

If nil, the bar will be made transparent and 1 pixel wide, as to be invisible,
but without sacrificing its ability to enforce `+header-line-height'.

Use `setq!' to adjust this variable live, as it will trigger an refresh of the
bars in the header-line. `setq' will not."
  :type 'integer
  :set #'+header-line--set-var-and-refresh-bars-fn)

(defvar +header-line-format-alist ()
  "An alist of header-line formats defined with `def-header-line!'.

Each entry's CAR is the name and CDR is a cons cell whose CAR is the left-hand
side of the header-line, and whose CDR is the right-hand side.")


;;
;;; Faces

(defface +header-line-bar-active '((t (:inherit highlight)))
  "Face used for left-most bar on the mode-line of an active window.")

(defface +header-line-bar-inactive '((t (:inherit mode-line-inactive)))
  "Face used for right-most bar on the mode-line of an inactive window.")

(defface +header-line-highlight
  '((t (:inherit mode-line-highlight)))
  "Face used for highlighted header-line panels (like search counts).")

(defface +header-line-alternate-highlight
  '((t (:inherit mode-line-highlight)))
  "Alternative face used for highlighted header-line panels (like search counts).")


;;
;;; Helpers

;;; `active'
(defvar +header-line--active-window (selected-window))

(defun +header-line-active ()
  "Return non-nil if the selected window has an active header-line."
  (eq (selected-window) +header-line--active-window))

(add-hook! 'pre-redisplay-functions
  (defun +header-line-set-selected-window-h (&rest _)
    "Track the active header-line's window in `+header-line--active-window'."
    (let ((win (selected-window)))
      (unless (minibuffer-window-active-p win)
        (setq +header-line--active-window (frame-selected-window))))))

(defun +header-line--make-xpm (color width height)
  "Create an XPM bitmap via COLOR, WIDTH and HEIGHT. Inspired by `powerline''s `pl/+header-line--make-xpm'."
  (propertize
   " " 'display
   (let ((data (make-list height (make-list width 1)))
         (color (or color "None")))
     (ignore-errors
       (create-image
        (concat
         (format "/* XPM */\nstatic char * percent[] = {\n\"%i %i 2 1\",\n\". c %s\",\n\"  c %s\","
                 (length (car data))
                 (length data)
                 color
                 color)
         (apply #'concat
                (cl-loop with idx = 0
                         with len = (length data)
                         for dl in data
                         do (cl-incf idx)
                         collect
                         (concat "\""
                                 (cl-loop for d in dl
                                          if (= d 0) collect (string-to-char " ")
                                          else collect (string-to-char "."))
                                 (if (eq idx len) "\"};" "\",\n")))))
        'xpm t :ascent 'center)))))

(defun +header-line-format-icon (icon label &optional face help-echo voffset)
  (propertize (concat (all-the-icons-material
                       icon
                       :face face
                       :height 1.1
                       :v-adjust (or voffset -0.225))
                      (propertize label 'face face))
              'help-echo help-echo))

(defun set-header-line! (name &optional default)
  "Set the header-line to NAME.
If DEFAULT is non-nil, apply to all future buffers. Header-Lines are defined with
`def-header-line!'."
  (if-let (format (assq name +header-line-format-alist))
      (cl-destructuring-bind (lhs . rhs) (cdr format)
        (if default
            (setq-default +header-line-format-left lhs
                          +header-line-format-right rhs)
          (setq +header-line-format-left lhs
                +header-line-format-right rhs)))
    (error "Could not find %S header-line format" name)))

(defun set-header-line-hook! (hooks name)
  "Set the header-line to NAME on HOOKS.
See `def-header-line!' on how header-lines are defined."
  (let ((fn (intern (format "+header-line-set-%s-format-h" name))))
    (dolist (hook (doom-enlist hooks))
      (when after-init-time
        (dolist (name (mapcar #'car +header-line-format-alist))
          (remove-hook hook (intern (format "+header-line-set-%s-format-h" name)))))
      (add-hook hook fn))))

(defun def-header-line! (name lhs rhs)
  "Define a header-line format by NAME.
LHS and RHS are the formats representing the left and right hand side of the
mode-line, respectively. See the variable `format-mode-line' for details on what
LHS and RHS will accept."
  (setf (alist-get name +header-line-format-alist)
        (cons lhs rhs))
  (fset (intern (format "+header-line-set-%s-format-h" name))
        (lambda (&rest _) (set-header-line! name))))

(defmacro def-header-line-var! (name body &optional docstring &rest plist)
  "TODO"
  (unless (stringp docstring)
    (push docstring plist)
    (setq docstring nil))
  `(progn
     (,(if (plist-get plist :local) 'defvar-local 'defvar)
      ,name ,body ,docstring)
     (put ',name 'risky-local-variable t)))

;;
;;; Segments

(def-header-line-var! +header-line-format-left nil
  "The left-hand side of the header-line."
  :local t)

(def-header-line-var! +header-line-format-right nil
  "The right-hand side of the header-line."
  :local t)


;;; `+header-line-bar'
(progn
  (def-header-line-var! +header-line-bar " ")
  (def-header-line-var! +header-line-inactive-bar " ")

  (add-hook! '(doom-init-ui-hook doom-load-theme-hook) :append
    (defun +header-line-refresh-bars-h ()
      (let ((width (or +header-line-bar-width 1))
            (height (max +header-line-height 0)))
        (setq +header-line-bar
              (+header-line--make-xpm
               (and +header-line-bar-width
                    (face-background '+header-line-bar-active nil t))
               width height)
              +header-line-inactive-bar
              (+header-line--make-xpm
               (and +header-line-bar-width
                    (face-background '+header-line-bar-active nil t))
               width height)))))

  (add-hook! 'doom-change-font-size-hook
    (defun +header-line-adjust-height-h ()
      (defvar +header-line--old-height +header-line-height)
      (let ((default-height +header-line--old-height)
            (scale (or (frame-parameter nil 'font-scale) 0)))
        (setq +header-line-height
              (if (> scale 0)
                  (+ default-height (* (or (frame-parameter nil 'font-scale) 1)
                                       doom-font-increment))
                default-height))
        (when doom-init-time
          (+header-line-refresh-bars-h))))))

;;
;;; Default header-line

(def-header-line! :empty nil nil)
(def-header-line! :main
  '("" +header-line-bar " ")
  '(""
    (:eval (evertedsphere/capture-shell-output "mpc-now-playing"))
    " "
    (:eval
     (if (libmpdel-playing-p)
         (+header-line-format-icon "play_arrow" "" 'doom-modeline-highlight "playing")
       (+header-line-format-icon "pause" "" 'doom-modeline-highlight "paused")))
    " "))

;;
;;; Bootstrap

(setq-default
 header-line-format
 '(""
   +header-line-format-left
   (:eval
    (propertize
     " "
     'display
     `((space :align-to (- (+ right right-fringe right-margin)
                           ,(string-width
                             (format-mode-line '("" +header-line-format-right))))))))
   +header-line-format-right))
(with-current-buffer "*Messages*"
  (setq header-line-format (default-value 'header-line-format)))


;; Other modes
(set-header-line-hook! '(text-mode-hook prog-mode-hook) :main)
