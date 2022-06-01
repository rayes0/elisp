:;exec emacs -batch -l "$0" -f main "$@"

(require 'package)
(require 'lisp-mnt)
(require 'org)

(defsubst insert-org-heading-with (text &optional top)
  (org-insert-heading nil nil top)
  (insert text)
  (insert "\n"))

(defsubst insert-org-subheading-with (text)
  (let ((org-todo-line-regexp "^\\(\\*+\\)\\(?: +\\(DONE\\|TODO\\)\\)?\\(?: +\\(.*?\\)\\)?[ 	]*$"))
    (org-insert-heading)
    (org-do-demote))
  (insert text)
  (insert "\n"))

(defun main ()
  (let ((readme-file (file-name-concat (getenv "GITHUB_WORKSPACE")
                                       "readme.org"))
        (package-list (directory-files (getenv "GITHUB_WORKSPACE") 'full "^.*\\.el$")))
    (delete-file readme-file)
    (with-temp-buffer
      (insert-preamble)
      (insert-toc package-list)
      (insert-org-heading-with "Packages" 'top)
      (dolist (pkg package-list)
        (insert-package-desc pkg))
      (write-region (point-min) (point-max) readme-file))))

(defun insert-preamble ()
  (insert
   "#+begin_quote
Everything looks like magic when you don’t understand it.

  --- Catherynne M. Valente, Afterword in /The Melancholy of Mechagirl/
#+end_quote
")
  (insert "Note: This readme is ")
  (org-insert-link nil "https://github.com/rayes0/elisp/blob/main/.github/make-readme.el"
                   "automatically generated")
  (insert ".\n")
  (insert-org-heading-with "General Notes")
  (insert "- Installation :: I recommend either cloning the repo and running one of the ~package-install-*~ functions, or using ")
  (org-insert-link nil "https://github.com/quelpa/quelpa" "quelpa")
  (insert ". This readme contains plug-and-play quelpa recipes for convenience.\n")
  (insert "- Customization :: Almost all the packages that are configurable have documented options organized into their groups. Check ~M-x customize-group RET PACKAGE_NAME~ for details.")
  (insert "\n\n")
  (insert "/n.b./ Some of these packages either depend on or are heavily enhanced by things part of very new emacs versions only (namely vtable, found in unreleased emacs 29). You may have to pull some things from newer emacs source code and install them yourself if you are using an older emacs.\n\n"))

(defun insert-toc (pkgs)
  (insert-org-heading-with "Contents")
  (dolist (pkg pkgs)
    (let* ((desc (get-desc pkg))
           (name (symbol-name (package-desc-name desc)))
           (summary (package-desc-summary desc)))
      (insert (format "- [[#%s][ ~%s~ ]] - %s\n" name name summary))))
  (insert "\n"))

(defun insert-package-desc (file)
  (let* ((desc (get-desc file))
         (name (symbol-name (package-desc-name desc)))
         (summary (package-desc-summary desc)))
    (insert-org-subheading-with name)
    (insert (format "| summary | %s |\n" summary))
    (insert (format "| quelpa  | ~(quelpa '(%s :fetcher github :repo \"rayes0/elisp\" :files (\"%s.el\")))~ |\n"
                    name name))
    (insert (or (lm-commentary file) ""))
    (unless (looking-at-p "\n") (insert "\n"))))

(defun get-desc (file)
  (with-temp-buffer
    (insert-file-contents file nil nil nil t)
    (package-buffer-info)))
