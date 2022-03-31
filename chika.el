;;; chika.el --- transient interface for linux package management  -*- lexical-binding: t; flycheck-disabled-checkers: (emacs-lisp-checkdoc); -*-
;; Version: 0.1

;; Package-Requires: ((transient "0.3.0"))

;;; Commentary:
;; An simple interface for package management on linux from Emacs with <https://github.com/magit/transient>.
;; It will run a execute a package manager and insert output into a buffer.
;; It will ask for root when performing certain actions, using TRAMP when appropriate.
;;
;; Mainly written for:
;;   - quickly toggling flags without typing shell commands
;;   - Emacs interface
;;   - no dependence on the current shell as it executes processes directly (no messing with quoting, etc.)
;;
;; currently implemented package managers: dnf, rpm, flatpak

;;; Code:

(require 'transient)
(require 'shell) ;; for coloring output

(defgroup chika nil
  "Transient interface for linux package management."
  :group 'external)

;; (defcustom chika-package-managers '(dnf rpm flatpak)
;;   "List of enabled package managers in the main `chika-dispatch' menu."
;;   :group 'chika)

(defcustom chika-tramp-method "/doas::"
  "Method string to use for tramp.
No trailing slash needed."
  :type 'string
  :group 'chika)

(defcustom chika-executables (list `(dnf . ,(executable-find "dnf"))
                                   `(rpm . ,(executable-find "rpm"))
                                   `(flatpak . ,(executable-find "flatpak")))
  "Locations of the executables to be used."
  :type '(alist :key-type symbol :value-type string)
  :group 'chika)

(defcustom chika-confirm-permissions t
  "Non-nil to confirm when executing with root permissions.a

It is highly recommended to leave this non-nil."
  :type 'boolean
  :group 'chika)

(defun chika-execute (command action flags &optional root)
  "Execute COMMAND with ACTION and FLAGS in current directory.
If ROOT is non-nil, execute with root permissions using Tramp."
  (if root (or (yes-or-no-p (format "Executing with root permissions using: %s. Continue? "
                                    chika-tramp-method))
               (user-error "chika: aborted!")))
  (with-current-buffer (get-buffer-create (format "*chika-%s-%s*" command action))
    (let ((default-directory (if root
                                 (format "%s/%s" chika-tramp-method (getenv "PWD"))
                               (getenv "PWD")))
          (args (if flags
                    (append (list action) flags)
                  (list action))))
      (erase-buffer)
      (shell-mode)
      (pop-to-buffer (current-buffer))
      (let ((process (apply 'start-file-process
                            (format "chika-%s" command)
                            (current-buffer)
                            (or (alist-get command chika-executables)
                                (error "Couldn't find executable. Check value of `chika-executables"))
                            args)))
        (set-process-sentinel process #'chika--process-event-handler)))))

(defun chika--process-event-handler (proc event)
  (message "chika: %s: %s" proc event))

;; dnf
;;;###autoload
(transient-define-prefix chika-dnf-transient ()
  "dnf"
  [["Commands"
    ("u" "upgrade" chika-dnf-upgrade)
    ("i" "install" chika-dnf-install)]
   ["Arguments"
    ("q" "quiet" "--quiet")]])

(defun chika-dnf-upgrade (&optional args)
  "Run dnf upgrade."
  (interactive (list (transient-args transient-current-command)))
  (chika-execute 'dnf "upgrade" args t))

(defun chika-dnf-install (package &optional args)
  "Run dnf install."
  (interactive (list (read-string "package: ")
                     (transient-args transient-current-command)))
  (unless (not (string= package ""))
    (user-error "chika: I need a package!"))
  (chika-execute 'dnf "install" (if args
                                    (append (list package) args)
                                  (list package))
                 t))

;; flatpak
;;;###autoload
(transient-define-prefix chika-flatpak-transient ()
  "flatpak"
  [["Commands"
    ("u" "upgrade" chika-flatpak-upgrade)
    ("i" "install" chika-flatpak-install)]])

(defun chika-flatpak-upgrade (&optional args)
  "Run flatpak upgrade."
  (interactive (transient-args transient-current-command))
  (chika-execute 'flatpak "upgrade" args))

(defun chika-flatpak-install (package &optional args)
  "Run flatpak install."
  (interactive (list (read-string "package: ")
                     (transient-args transient-current-command)))
  (unless (not (string= package ""))
    (user-error "chika: I need a package!"))
  (chika-execute 'flatpak "install" (if args
                                        (append (list package) args)
                                      (list package))
                 nil))

;; rpm
;;;###autoload
(transient-define-prefix chika-rpm-transient ()
  "rpm"
  ["u" "test"])

;; Top level interface
;;;###autoload
(transient-define-prefix chika-dispatch ()
  "Invoke a package manager command."
  ["Choose package manager"
   ("d" "dnf" chika-dnf-transient)
   ("f" "flatpak" chika-flatpak-transient)
   ("r" "rpm" chika-rpm-transient)])

;; (defun chika--get-menu ()
;;   (if chika-package-managers
;;       (let ((vec ["Choose package manager"]))
;;         (dolist (p chika-package-managers)
;;           (setq vec (vconcat vec
;;                              (cond ((eq p 'dnf) '("d" "dnf" chika-dnf-transient))
;;                                    ((eq p 'flatpak) '("f" "flatpak" chika-flatpak-transient))
;;                                    ((eq p 'rpm) '("r" "rpm" chika-rpm-transient))
;;                                    (t (message "chika: `%s' is not a supported package manager"
;;                                                p))))))
;;         vec)
;;     (error "`chika-package-managers' is nil.")))

;;;###autoload
(defun chika ()
  "Run the chika dispatcher"
  (interactive)
  (chika-dispatch))

(provide 'chika)

;;; chika.el ends here