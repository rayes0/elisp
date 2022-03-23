;;; cmus.el --- Very simple interface to control cmus from emacs    -*- lexical-binding: t -*-

;; Author: rayes
;; Package-Requires: ((vterm "1.0"))
;; URL: https://github.com/rayes0/elisp

;;; Code:

(require 'vterm)

(defcustom
    cmus-command (executable-find "cmus")
  "Path to cmus executable."
  :group 'cmus
  :type 'string)

(defcustom
  cmus-remote-command (executable-find "cmus-remote")
  "Path to cmus-remote executable."
  :group 'cmus
  :type 'string)

(defvar cmus-global-buffer)

(defun cmus-play-pause ()
  "Toggle between play and pause."
  (interactive)
  (call-process cmus-remote-command nil nil nil "-u"))

(defun cmus-stop ()
  "Stop playing music (doesn't exit cmus though)."
  (interactive)
  (call-process cmus-remote-command nil nil nil "-s"))

(defun cmus-next ()
  "Skip to next song."
  (interactive)
  (call-process cmus-remote-command nil nil nil "-n"))

(defun cmus-previous ()
  "Go to previous song."
  (interactive)
  (call-process cmus-remote-command nil nil nil "-r"))

(defun cmus-vterm-start ()
  "Create a new vterm buffer for cmus."
  (interactive)
  (setq cmus-global-buffer (generate-new-buffer "*cmus - vterm*"))
  (when (buffer-live-p cmus-global-buffer)
    (let ((vterm-shell cmus-command)
          (vterm-kill-buffer-on-exit t))
      (with-current-buffer cmus-global-buffer
        (vterm-mode)
        (local-set-key (kbd "C-c C-k") 'bury-buffer))
      (switch-to-buffer cmus-global-buffer))))

(defun cmus-vterm-start-bg ()
  "Run cmus-vterm-start and then bury the cmus buffer."
  (interactive)
  (cmus-vterm-start)
  (bury-buffer cmus-global-buffer))

(defun cmus-view ()
  "Switch to the cmus buffer."
  (interactive)
  (when (buffer-live-p cmus-global-buffer)
    (switch-to-buffer cmus-global-buffer)))

(defun cmus-setup-default ()
  "Setup prefix command and default keybinds for cmus."
  (interactive)
  (define-prefix-command 'cmus-map)
  (global-set-key (kbd "C-c C-v m") 'cmus-map)
  (define-key cmus-map (kbd "f") 'cmus-view)
  (define-key cmus-map (kbd "p") 'cmus-play-pause)
  (define-key cmus-map (kbd "s") 'cmus-stop)
  (define-key cmus-map (kbd "n") 'cmus-next)
  (define-key cmus-map (kbd "r") 'cmus-previous)
  (define-key cmus-map (kbd "q") 'cmus-status))
  
(provide 'cmus)

;;; cmus.el ends here
