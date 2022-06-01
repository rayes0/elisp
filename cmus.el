;;; cmus.el --- very simple interface to control cmus from emacs    -*- lexical-binding: t -*-
;; Version: 0.1
;; Package-Requires: ((vterm "1.0"))

;; Author: rayes
;; URL: https://github.com/rayes0/elisp

;;; Commentary:
;; Couple functions to run cmus from emacs with vterm,
;; and do all the usual things a music player can do.

;;; Code:

(require 'vterm)

(defcustom cmus-command (executable-find "cmus")
  "Path to cmus executable."
  :group 'cmus
  :type 'string)

(defcustom cmus-remote-command (executable-find "cmus-remote")
  "Path to cmus-remote executable."
  :group 'cmus
  :type 'string)

(defvar cmus-global-buffer)

(defun cmus-play-pause ()
  "Toggle between play and pause."
  (interactive)
  (when (cmus-running-p)
    (call-process cmus-remote-command nil nil nil "-u")))

(defun cmus-stop ()
  "Stop playing music (doesn't exit cmus though)."
  (interactive)
  (when (cmus-running-p)
    (call-process cmus-remote-command nil nil nil "-s")))

(defun cmus-next ()
  "Skip to next song."
  (interactive)
  (when (cmus-running-p)
    (call-process cmus-remote-command nil nil nil "-n")))

(defun cmus-previous ()
  "Go to previous song."
  (interactive)
  (when (cmus-running-p)
    (call-process cmus-remote-command nil nil nil "-r")))

;;;###autoload
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

;;;###autoload
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

(defun cmus-running-p ()
  "Return t is cmus is running."
  (when (get-buffer-process cmus-global-buffer) t))

(defun cmus-playing-p ()
  "Return t if cmus is both currently running and playing."
  (when (cmus-running-p)
    (with-temp-buffer
      (erase-buffer)
      (call-process cmus-remote-command nil t nil "-Q")
      (when (string-match-p "^status playing\n.*" (buffer-string)) t))))

;; (defun cmus-current-song ()
;;   "Return string of the currently active song."
;;   (when (cmus-running-p)
;;     (with-temp-buffer
;;       (erase-buffer)
;;       (call-process cmus-remote-command nil t nil "-Q")
;;       )))

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
