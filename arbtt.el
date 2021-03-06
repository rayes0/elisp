;;; arbtt.el --- view arbtt stats from emacs    -*- lexical-binding: t -*-
;; Version: 0.1
;; Package-Requires: ((emacs "29"))

;; Author: rayes
;; URL: https://github.com/rayes0/elisp

;;; Commentary:
;; Simple way to view [[https://github.com/nomeata/arbtt][arbtt]] stats from emacs.
;; Currently very primitive. Runs arbtt with csv output, and parses that into a table.

;;; Code:

(require 'vtable)
(eval-when-compile (require 'subr-x))

(defgroup arbtt nil
  "View arbtt stats from emacs."
  :group 'external)

(defcustom arbtt-stats-executable (executable-find "arbtt-stats")
  "Location of the `arbtt-stats' executable."
  :type 'string
  :group 'arbtt)

(defcustom arbtt-stats-flags nil
  "List of strings containing flags to pass when calling arbtt-stats."
  :type 'list
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
    (let ((inhibit-read-only t))
      (erase-buffer)
      (make-vtable
       :columns (car parsed)
       :objects (cdr parsed)
       :keymap (define-keymap
                 "q" #'bury-buffer
                 "s" #'isearch-forward
                 "t" #'arbtt-filter-tag
                 "c" #'arbtt-filter-category)))))

(defun arbtt-filter-category (cat)
  "Interactively filter a tag in the arbtt buffer."
  (interactive "MEnter category: ")
  (if cat (arbtt (concat "--category=" cat))))

(defun arbtt-filter-tag (tag)
  "Interactively filter a tag in the arbtt buffer."
  (interactive "MEnter tag: ")
  (if tag (arbtt (concat "--only=" tag))))

;;;###autoload
(defun arbtt (&optional flags)
  "View arbtt stats."
  (interactive)
  (with-current-buffer (get-buffer-create "*arbtt-debug*")
    (erase-buffer)
    (make-process :name "arbtt"
                  :buffer (current-buffer)
                  :command (append (list arbtt-stats-executable)
                                   (cond ((and flags arbtt-stats-flags)
                                          (seq-concatenate 'list (list flags) arbtt-stats-flags))
                                         (flags (list flags))
                                         (arbtt-stats-flags arbtt-stats-flags)
                                         (t ()))
                                   '("--output-format=csv"))
                  :filter #'arbtt--filter
                  :stderr (get-buffer-create "*arbtt-debug*")))
  (with-current-buffer (get-buffer-create "*arbtt*")    
    (view-mode 1)
    ;; (read-only-mode 1)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert "processing..."))
    (pop-to-buffer-same-window (current-buffer))))

(provide 'arbtt)

;;; arbtt.el ends here
