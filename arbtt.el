;;; arbtt.el --- view arbtt stats from emacs    -*- lexical-binding: t -*-

;; Author: rayes
;; Package-Requires: ((emacs "29.1") vtable)
;; URL: https://github.com/rayes0/elisp

;;; Code:

(require 'vtable)
(require 'subr-x)

(defgroup arbtt nil
  "View arbtt stats from emacs."
  :group 'external)

(defcustom arbtt-stats-executable (executable-find "arbtt-stats")
  "Location of the `arbtt-stats' executable."
  :type 'string
  :group 'arbtt)

(defcustom arbtt-stats-flags nil
  "List of strings containing flags to pass when calling arbtt-stats."
  :group 'arbtt)

(defun arbtt--filter (proc output)
  (let* ((procbuf (process-buffer proc))
         (lines (split-string (string-trim output) "\n"))
         (parsed (cl-loop for l in lines
                       collect (split-string l ","))))
    (when (buffer-live-p procbuf)
      (arbtt--contents-insert (get-buffer-create "*arbtt*") parsed))))

(defun arbtt--contents-insert (buf parsed)
  (with-current-buffer buf
    (erase-buffer)
    (make-vtable
     :columns (car parsed)
     :objects (cdr parsed)
     :keymap (define-keymap
                 "q" #'bury-buffer
               "s" #'isearch-forward
               "t" #'arbtt-filter-tag
               "c" #'arbtt-filter-category))))

(defun arbtt-filter-category (cat)
  "Interactively filter a tag in the arbtt buffer."
  (interactive "MEnter category: ")
  (if cat (arbtt (concat "--category=" cat))))

(defun arbtt-filter-tag (tag)
  "Interactively filter a tag in the arbtt buffer."
  (interactive "MEnter tag: ")
  (if tag (arbtt (concat "--only=" tag))))

(defun arbtt (&optional flags)
  "View arbtt stats."
  (interactive)
  (with-current-buffer (get-buffer-create "*arbtt-debug*")
    (erase-buffer)
    (make-process :name "arbtt"
                  :buffer (current-buffer)
                  :command (append (cond ((and flags arbtt-stats-flags) (list arbtt-stats-executable
                                                                              flags
                                                                              arbtt-stats-flags))
                                         (flags (list arbtt-stats-executable flags))
                                         (arbtt-stats-flags (list arbtt-stats-executable
                                                                  arbtt-stats-flags))
                                         (t (list arbtt-stats-executable)))
                                   '("--output-format=csv"))
                  :filter #'arbtt--filter
                  :stderr (get-buffer-create "*arbtt-debug*")))
  (with-current-buffer (get-buffer-create "*arbtt*")
    (erase-buffer)
    (view-mode 1)
    (insert "processing...")
    (pop-to-buffer-same-window (current-buffer))))

(provide 'arbtt)

;;; arbtt.el ends here
