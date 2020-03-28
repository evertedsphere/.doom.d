;;; ui/maxibuffer/config.el -*- lexical-binding: t; -*-

;; Filename: maxibuffer.el
;; Description: Modular tray bar
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2018, Andy Stewart, all rights reserved.
;; Created: 2018-10-07 07:30:16
;; Version: 3.9
;; Last-Updated: 2020-02-27 20:06:54
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/maxibuffer.el
;; Keywords:
;; Compatibility: GNU Emacs 27.0.50
;;
;; Features that might be required by this library:
;;
;; `cl-lib'
;; `subr-x'
;; `battery'
;;

;;; Customize:
;;
;; `maxibuffer-mode-line-active-color'
;; `maxibuffer-mode-line-inactive-color'
;; `maxibuffer-active-modules'
;; `maxibuffer-git-update-duration'
;;
;; All of the above can customize by:
;;      M-x customize-group RET maxibuffer RET
;;

(require 'cl-lib)
(require 'subr-x)
(require 'battery)

;;; Code:
(defgroup maxibuffer nil
  "Modular tray bar."
  :group 'maxibuffer)

(defcustom maxibuffer-active-modules
  '("location" "parent-dir" "mode-name" "awesome-tab" "battery" "date")
  "Default active modules."
  :type 'list
  :group 'maxibuffer)

(defcustom maxibuffer-git-update-duration 5
  "Update duration of git command, in seconds.

It's very slow start new process in Windows platform.
Maybe you need set this option with bigger value to speedup on Windows platform."
  :type 'integer
  :group 'maxibuffer)

(defcustom maxibuffer-battery-update-duration 5
  "Update duration of battery status, in seconds.

It will make command `set-mark-command' failed if not use duration."
  :type 'integer
  :group 'maxibuffer)

(defcustom maxibuffer-file-path-show-filename nil
  "Show filename in file-path module or not."
  :type 'boolean
  :group 'maxibuffer)

(defcustom maxibuffer-file-path-truncated-name-length 1
  "In file-path module, how many letters to leave when truncate dirname.

Beginning dots are not counted."
  :type 'integer
  :group 'maxibuffer)

(defcustom maxibuffer-file-path-full-dirname-levels 2
  "In file-path module, how many levels of parent directories should be shown in
their full name."
  :type 'integer
  :group 'maxibuffer)

(defcustom maxibuffer-file-path-truncate-dirname-levels 0
  "In file-path module, how many levels of parent directories should be shown in
their first character.

These goes before those shown in their full names."
  :type 'integer
  :group 'maxibuffer)

(defface maxibuffer-module-git-face
  '((((background light))
     :foreground "#cc2444" :bold t)
    (t
     :foreground "#ff2d55" :bold t))
  "Git face."
  :group 'maxibuffer)

(defface maxibuffer-module-rvm-face
  '((((background light))
     :foreground "#2832cc" :bold t)
    (t
     :foreground "#333fff" :bold t))
  "RVM face."
  :group 'maxibuffer)

(defface maxibuffer-module-circe-face
  '((((background light))
     :foreground "#2832cc" :bold t)
    (t
     :foreground "#333fff" :bold t))
  "Circe face."
  :group 'maxibuffer)

(defface maxibuffer-module-mode-name-face
  '((((background light))
     :foreground "#00a400" :bold t)
    (t
     :foreground "green3" :bold t))
  "Mode name face."
  :group 'maxibuffer)

(defface maxibuffer-module-location-face
  '((((background light))
     :foreground "#cc7700" :bold t)
    (t
     :foreground "#ff9500" :bold t))
  "Location face."
  :group 'maxibuffer)

(defface maxibuffer-module-time-face
  '((t (:inherit 'doom-modeline-buffer-major-mode)))
  "Date face."
  :group 'maxibuffer)

(defface maxibuffer-module-date-face
  '((t (:inherit 'doom-modeline-buffer-minor-mode)))
  "Date face."
  :group 'maxibuffer)

(defface maxibuffer-module-last-command-face
  '((((background light))
     :foreground "#0061cc" :bold t)
    (t
     :foreground "#007aff" :bold t))
  "Date face."
  :group 'maxibuffer)

(defface maxibuffer-module-buffer-name-face
  '((((background light))
     :foreground "#cc7700" :bold t)
    (t
     :foreground "#ff9500" :bold t))
  "Buffer name face."
  :group 'maxibuffer)

(defface maxibuffer-module-parent-dir-face
  '((((background light))
     :foreground "#5e8e2e" :bold t)
    (t
     :foreground "#9ded4d" :bold t))
  "Parent dir face."
  :group 'maxibuffer)

(defface maxibuffer-module-file-path-face
  '((((background light))
     :foreground "#5e8e2e" :bold t)
    (t
     :foreground "#9ded4d" :bold t))
  "Parent dir face."
  :group 'maxibuffer)

(defface maxibuffer-module-awesome-tab-face
  '((((background light))
     :foreground "#b83059" :bold t)
    (t
     :foreground "#e73c70" :bold t))
  "Awesome tab face."
  :group 'maxibuffer)

(defface maxibuffer-module-evil-face
  '((((background light))
     :foreground "#008080" :bold t)
    (t
     :foreground "#00ced1" :bold t))
  "Evil state face."
  :group 'maxibuffer)

(defface maxibuffer-module-battery-face
  '((t (:inherit 'doom-modeline-info)))
  "Battery state face."
  :group 'maxibuffer)

(define-minor-mode maxibuffer-mode
  "Modular tray bar."
  :require 'maxibuffer-mode
  :global t
  (if maxibuffer-mode
      (maxibuffer-enable)
    (maxibuffer-disable)))

(defvar maxibuffer-info-padding-right 1)

(defvar maxibuffer-mode-line-colors nil)

(defvar maxibuffer-timer nil)

(defvar maxibuffer-active-p nil)

(defvar maxibuffer-git-command-last-time 0)

(defvar maxibuffer-git-command-cache "")

(defvar maxibuffer-battery-status-last-time 0)

(defvar maxibuffer-battery-status-cache "")

(defvar maxibuffer-last-tray-info nil)

(defvar maxibuffer-mode-line-default-height 1)

(defvar maxibuffer-module-alist
  '(("awesome-tab" . (maxibuffer-module-awesome-tab-info maxibuffer-module-awesome-tab-face))
    ("buffer-name" . (maxibuffer-module-buffer-name-info maxibuffer-module-buffer-name-face))
    ("circe" . (maxibuffer-module-circe-info maxibuffer-module-circe-face))
    ("date" . (maxibuffer-module-date-info maxibuffer-module-date-face))
    ("time" . (maxibuffer-module-time-info maxibuffer-module-time-face))
    ("evil" . (maxibuffer-module-evil-info maxibuffer-module-evil-face))
    ("file-path" . (maxibuffer-module-file-path-info maxibuffer-module-file-path-face))
    ("git" . (maxibuffer-module-git-info maxibuffer-module-git-face))
    ("last-command" . (maxibuffer-module-last-command-info maxibuffer-module-last-command-face))
    ("location" . (maxibuffer-module-location-info maxibuffer-module-location-face))
    ("parent-dir" . (maxibuffer-module-parent-dir-info maxibuffer-module-parent-dir-face))
    ("mode-name" . (maxibuffer-module-mode-name-info maxibuffer-module-mode-name-face))
    ("rvm" . (maxibuffer-module-rvm-info maxibuffer-module-rvm-face))
    ("battery" . (maxibuffer-module-battery-info maxibuffer-module-battery-face))))


(defun maxibuffer-enable ()
  ;; Add update timer.
  (setq maxibuffer-timer
        (run-with-timer 0 0.5 'maxibuffer-show-info))
  (add-hook 'focus-in-hook 'maxibuffer-show-info)
  (setq maxibuffer-active-p t))

(defun maxibuffer-disable ()
  ;; Cancel timer.
  (when (timerp maxibuffer-timer)
    (cancel-timer maxibuffer-timer))
  (remove-hook 'focus-in-hook 'maxibuffer-show-info)
  ;; Update mode-line.
  (force-mode-line-update)
  (redraw-display)
  (with-current-buffer " *Minibuf-0*"
    (erase-buffer))
  (setq maxibuffer-active-p nil))

(defun maxibuffer-build-info ()
  (condition-case nil
      (mapconcat 'identity (cl-remove-if #'(lambda (n) (equal (length n) 0))
                                         (mapcar 'maxibuffer-get-module-info maxibuffer-active-modules)) " ")
    (format "Awesome Tray broken.")))

(defun maxibuffer-get-module-info (module-name)
  (let* ((func (ignore-errors (cadr (assoc module-name maxibuffer-module-alist))))
         (face (ignore-errors (cddr (assoc module-name maxibuffer-module-alist))))
         (info (ignore-errors (propertize (funcall func) 'face face))))
    (if info
        info
      (propertize "" 'face face))))

(defun maxibuffer-module-git-info ()
  (if (executable-find "git")
      (let ((current-seconds (maxibuffer-current-seconds)))
        (if (> (- current-seconds maxibuffer-git-command-last-time) maxibuffer-git-update-duration)
            (progn
              (setq maxibuffer-git-command-last-time current-seconds)
              (maxibuffer-update-git-command-cache))
          maxibuffer-git-command-cache))
    ""))

(defun maxibuffer-module-circe-info ()
  "Display circe tracking buffers"
  (if (listp tracking-mode-line-buffers)
      (apply 'concat (cl-loop for entry in tracking-mode-line-buffers
                              collect (or (plist-get entry :propertize) "")))
    ""))

(defun maxibuffer-module-rvm-info ()
  (if (executable-find "rvm-prompt")
      (format "rvm:%s" (replace-regexp-in-string
                        "\n" ""
                        (nth 1 (maxibuffer-process-exit-code-and-output "rvm-prompt")))
              )
    ""))

(defun maxibuffer-module-battery-info ()
  (let ((current-seconds (maxibuffer-current-seconds)))
    (if (> (- current-seconds maxibuffer-battery-status-last-time) maxibuffer-battery-update-duration)
        (progn
          (setq maxibuffer-battery-status-last-time current-seconds)
          (setq maxibuffer-battery-status-cache (battery-format "%p%% (%t)" (funcall battery-status-function))))
      maxibuffer-battery-status-cache)))

(defun maxibuffer-module-mode-name-info ()
  (format "%s" major-mode))

(defun maxibuffer-module-location-info ()
  (format "(%s:%s %s)"
          (format-mode-line "%l")
          (format-mode-line "%c")
          (format-mode-line "%p")
          ))

(defun maxibuffer-module-date-info ()
  (format-time-string "%a %d %b %Y"))

(defun maxibuffer-module-time-info ()
  (format-time-string "%H:%M:%S"))

(defun maxibuffer-module-last-command-info ()
  (format "%s" last-command))

(defun maxibuffer-module-buffer-name-info ()
  (format "%s" (buffer-name)))

(defun maxibuffer-module-parent-dir-info ()
  (if (or (derived-mode-p 'dired-mode) (not buffer-file-name))
      ""
    (format "%sdir:%s"
            (if (buffer-modified-p) "*" "")
            (file-name-nondirectory (directory-file-name default-directory)))))

(defun maxibuffer-shrink-dir-name (name)
  "Shrink NAME to be its first letter, or the first two if starts \".\"

NAME is a string, typically a directory name."
  (let ((dot-num (if (string-match "^\\.+" name)
                     (length (match-string 0 name))
                   0)))
    (substring name 0 (min (length name) (+ dot-num maxibuffer-file-path-truncated-name-length)))))

(defun maxibuffer-module-file-path-info ()
  (if (not buffer-file-name)
      (format "%s" (buffer-name))
    (let* ((file-path (split-string (buffer-file-name) "/" t))
           (shown-path)
           (path-len (length file-path))
           (modp (if (buffer-modified-p) "*" ""))
           (full-num maxibuffer-file-path-full-dirname-levels)
           (trunc-num maxibuffer-file-path-truncate-dirname-levels)
           (show-name maxibuffer-file-path-show-filename))
      (when (> path-len (+ 1 full-num))
        (push (string-join
               (mapcar #'maxibuffer-shrink-dir-name
                       (cl-subseq file-path
                                  (max 0 (- path-len (+ 1 full-num trunc-num)))
                                  (- path-len (1+ full-num)))) "/")
              shown-path))
      (when (> path-len 1)
        (push (string-join
               (cl-subseq file-path
                          (max 0 (- path-len (1+ full-num)))
                          (1- path-len)) "/")
              shown-path))
      (when show-name
        (push (car (last file-path)) shown-path))
      (concat modp
              (if (<= path-len (+ 1 full-num trunc-num))
                  "/"
                ".../")
              (string-join (nreverse (cl-remove "" shown-path)) "/")
              (when (and shown-path (not show-name)) "/")))))

(defun maxibuffer-module-awesome-tab-info ()
  (with-demoted-errors
      ""
    (if (featurep 'awesome-tab)
        (let ((tab-info (format "%s" (cdr (awesome-tab-selected-tab (awesome-tab-current-tabset t))))))
          (if (> (string-width tab-info) 30)
              ""
            tab-info))
      "")))

(defun maxibuffer-module-evil-info ()
  (with-demoted-errors
      ""
    (if (featurep 'evil)
        (let ((state
               (cond ((evil-normal-state-p) "<N>")
                     ((evil-emacs-state-p) "<E>")
                     ((evil-insert-state-p) "<I>")
                     ((evil-motion-state-p) "<M>")
                     ((evil-visual-state-p) "<V>")
                     ((evil-operator-state-p) "<O>")
                     ((evil-replace-state-p) "<R>")
                     (t ""))))
          state)
      "")))

(defun maxibuffer-show-info ()
  ;; Only flush tray info when current message is empty.
  (unless (current-message)
    (maxibuffer-flush-info)))

(defun maxibuffer-get-frame-width ()
  "Only calculating a main Frame width, to avoid wrong width when new frame, such as `snails'."
  (with-selected-frame (car (last (frame-list)))
    (frame-width)))

(defun maxibuffer-flush-info ()
  (let* ((tray-info (maxibuffer-build-info)))
    (with-current-buffer " *Minibuf-0*"
      (erase-buffer)
      (insert (concat (make-string (max 0 (- (maxibuffer-get-frame-width) (string-width tray-info) maxibuffer-info-padding-right)) ?\ ) tray-info)))))

(defun maxibuffer-get-echo-format-string (message-string)
  (let* ((tray-info (maxibuffer-build-info))
         (blank-length (- (maxibuffer-get-frame-width) (string-width tray-info) (string-width message-string) maxibuffer-info-padding-right))
         (empty-fill-string (make-string (max 0 (- (maxibuffer-get-frame-width) (string-width tray-info) maxibuffer-info-padding-right)) ?\ ))
         (message-fill-string (make-string (max 0 (- (maxibuffer-get-frame-width) (string-width message-string) (string-width tray-info) maxibuffer-info-padding-right)) ?\ )))
    (prog1
        (cond
         ;; Fill message's end with whitespace to keep tray info at right of minibuffer.
         ((> blank-length 0)
          (concat message-string message-fill-string tray-info))
         ;; Fill empty whitespace if new message contain duplicate tray-info (cause by move mouse on minibuffer window).
         ((and maxibuffer-last-tray-info
               message-string
               (string-suffix-p maxibuffer-last-tray-info message-string))
          (concat empty-fill-string tray-info))
         ;; Don't fill whitepsace at end of message if new message is very long.
         (t
          (concat message-string "\n" empty-fill-string tray-info)))
      ;; Record last tray information.
      (setq maxibuffer-last-tray-info tray-info))))

(defun maxibuffer-process-exit-code-and-output (program &rest args)
  "Run PROGRAM with ARGS and return the exit code and output in a list."
  (with-temp-buffer
    (list (apply 'call-process program nil (current-buffer) nil args)
          (buffer-string))))

(defun maxibuffer-current-seconds ()
  (string-to-number (format-time-string "%s")))

(defun maxibuffer-update-git-command-cache ()
  (let* ((git-info (maxibuffer-process-exit-code-and-output "git" "symbolic-ref" "--short" "HEAD"))
         (status (nth 0 git-info))
         (result (format "git:%s" (nth 1 git-info))))
    (setq maxibuffer-git-command-cache
          (if (equal status 0)
              (replace-regexp-in-string "\n" "" result)
            ""))
    maxibuffer-git-command-cache))

;; Wrap `message' make tray information visible always
;; even other plugins call `message' to flush minibufer.
(defun maxibuffer-message-advice (old-message &rest arguments)
  (unless (ignore-errors
            (cond
             ;; Don't wrap tray info if `maxibuffer-active-p' is nil.
             ((not maxibuffer-active-p)
              (apply old-message arguments))
             ;; Don't wrap maxibuffer info if variable `inhibit-message' is non-nil.
             (inhibit-message
              (apply old-message arguments))
             ;; Just flush tray info if message string is empty.
             ((not (car arguments))
              (apply old-message arguments)
              (maxibuffer-flush-info))
             ;; Otherwise, wrap message string with tray info.
             (t
              (apply old-message "%s" (cons (maxibuffer-get-echo-format-string (apply 'format arguments)) '()))
              ))
            ;; Return t if everything is okay.
            t)
    (apply old-message "Sorry, something error in maxibuffer.")
    ))

(advice-add #'message :around #'maxibuffer-message-advice)

(defun maxibuffer-end-of-buffer-advice (old-func &rest arguments)
  (apply old-func arguments)
  (message ""))

(advice-add #'end-of-buffer :around #'maxibuffer-end-of-buffer-advice)

(defun maxibuffer-beginning-of-buffer-advice (old-func &rest arguments)
  (apply old-func arguments)
  (message ""))

(advice-add #'beginning-of-buffer :around #'maxibuffer-beginning-of-buffer-advice)
