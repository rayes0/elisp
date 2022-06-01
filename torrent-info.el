;;; torrent-info.el --- show info from torrent files  -*- lexical-binding: t; -*-
;; Version: 1.0
;; Package-Requires: ((bencoding) (emacs "29"))

;; Author: rayes
;; URL: https://github.com/rayes0/elisp

;;; Commentary:
;; Various useful things for working with torrent files.
;; It currently doesn't contain much

;;; Code:

(require 'bencoding)
(require 'vtable)

(defun torrent-info-file (file)
  "Choose a torrent FILE to show info."
  (interactive "fChoose file: \n")
  (let* ((data (bencoding-read-file file))
         (inhibit-read-only t)
         (buffer (get-buffer-create "*torrent-info*"))
         (files-p (map-nested-elt data '("info" "files")))
         (info (assoc-delete-all "files" (map-elt data "info"))))
    (with-current-buffer buffer
      (read-only-mode 1)
      (erase-buffer)
      (make-vtable :columns `("File: " ,file)
                   :objects (cl-concatenate 'list
                                            (assoc-delete-all "pieces" info)
                                            (assoc-delete-all "info" data))
                   :separator-width 2
                   :use-header-line nil
                   :getter (lambda (object column _table)
                             (pcase column
                               (0 (car object))
                               (1 (pcase (car object)
                                    ((or "length"
                                         "piece length")
                                     (format "%s [%s]" (file-size-human-readable (cdr object)) (cdr object)))
                                    ("creation date" (format-time-string "%D" (cdr object)))
                                    ("private" (if (eq (cdr object) 1) "yes" "no"))
                                    (_ (cdr object)))))))
      (when files-p
        (goto-char (point-max))
        (insert "\n\n")
        (make-vtable :columns '("Filelist" "")
                     :objects files-p
                     :use-header-line nil
                     :getter (lambda (o c _)
                               (pcase c
                                 (0 (car (alist-get "path" o nil nil #'equal)))
                                 (1 (format "%s [%s]"
                                            (file-size-human-readable
                                             (alist-get "length" o nil nil #'equal))
                                            (alist-get "length" o nil nil #'equal))))))))
    (pop-to-buffer-same-window buffer)))

(provide 'torrent-info)

;;; torrent-info.el ends here