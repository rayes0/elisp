;;; arietta.el --- simple interface for the aria2 daemon.  -*- lexical-binding: t; -*-
;; Version: 1.0
;; Package-Requires: ((emacs "26.1") (json "1.5") (websocket "1.13"))

;; Author: rayes
;; URL: https://github.com/rayes0/elisp

;;; Commentary:
;; (Nearly) fully featured interface for the aria2 daemon over websocket.
;;
;; To use, you will need to start aria2 as a daemon with rpc enabled.
;; Check the aria2 manpage for more details.

;;; Code:

(require 'websocket)
(require 'json)
(require 'jsonrpc)
(require 'cl-lib)
(require 'tabulated-list)
(require 'thunk)
(require 'notifications)
(require 'tramp)
;; (require 'taxy)
;; (require 'taxy-magit-section)

(when (featurep 'vtable)
  (require 'vtable))

(defgroup arietta nil
  "Interface for the aria2 daemon."
  :group 'external)

(defcustom arietta-rpc-hostname "localhost"
  "Hostname for accessing aria2c RPC."
  :type 'string
  :group 'arietta)

(defcustom arietta-rpc-port 6800
  "Port for accessing aria2c RPC."
  :type 'integer
  :group 'arietta)

(defcustom arietta-rpc-secret nil
  "Token to use for connection.

If nil, don't attempt to use a token.
Using a token is recommended, see the aria2 manpage for details."
  :type 'string
  :group 'arietta)

(defcustom arietta-use-tls t
  "Whether to encrypt connections via SSL or TLS (or WSS for websocket).
Requires that you configure aria2 to use `--rpc-secure=true'"
  :type 'bool
  :group 'arietta)

(defcustom arietta-refresh-interval 1.0
  "Refresh interval for when the arietta buffer is open."
  :type 'float
  :group 'arietta)

(defcustom arietta-fetch-num 100
  "Number of inactive (waiting or stopped) downloads to fetch.

A higher number will be slower if you have a lot of downloads.
All active downloads are always fetched."
  :type 'integer
  :group 'arietta)

(defcustom arietta-prompt-directory nil
  "Directory to default to when prompting for download dir.

Nil to use current directory."
  :type 'string
  :group 'arietta)

(defcustom arietta-bitfield-divisions 80
  "Number of divisons to use for displaying the bitfield in bittorrent downloads."
  :type 'integer
  :group 'arietta)

;; (defcustom arietta-protocol 'websocket
;;   "What protocol to use. Either `http' or `websocket'.

;; TLS or WSS can be enabled with the `arietta-use-tls' option."
;;   :type '(choice (const http)
;;                  (const websocket))
;;   :group 'arietta)


(defvar arietta--rpc nil "The aria2c RPC controller.")
(defvar arietta--refresh-timer nil)
(defvar arietta--list-buffer nil)
(defvar arietta--info-buffer nil)
(defvar arietta--pending-update (make-mutex))

(defclass arietta-rpc ()
  ((-websocket
    :initarg :websocket
    :accessor arietta--websocket)
   (data
    :initform ()
    :accessor arietta--data)
   ;; (status-data
   ;;  :accessor arietta--status)
   ;; (global-stats
   ;;  :accessor arietta--global-stats)
   )
  "Controller and data for an aria2 RPC connection.")

(cl-defmethod arietta-connection-send ((connection arietta-rpc)
                                       &rest args
                                       &key _id method params)
  "Send ARGS as JSON to the aria2c endpoint CONNECTION via websocket."
  (arietta--ensure)
  (when method
    (plist-put args :method
               (cond ((keywordp method) (substring (symbol-name method) 1))
                     ((symbolp method) (symbol-name method)))))
  (if arietta-rpc-secret
      (plist-put args :params
                 (vconcat (vector (format "token:%s" arietta-rpc-secret)) params)))
  (websocket-send-text (arietta--websocket connection)
                       (jsonrpc--json-encode `(:jsonrpc "2.0" ,@args))))

(cl-defmethod arietta-websocket-p ((connection arietta-rpc))
  "Return t if the websocket in CONNECTION is up."
  (if (websocket-openp (arietta--websocket connection))
      t
    nil))

(defsubst arietta--size (size-str)
  "Convert SIZE-STR to more friendly value."
  (file-size-human-readable (string-to-number size-str)))

(cl-defmethod arietta--frame-handler ((connection arietta-rpc) frame)
  "Handle a message FRAME from websocket in CONNECTION."
  (thunk-let* ((json (json-parse-string (websocket-frame-text frame)
                                        ;; probably faster than hash table for short lists
                                        :object-type 'plist
                                        :array-type 'list))
               (error-maybe (plist-get json :error))
               (result (plist-get json :result))
               (-id (plist-get json :id)))
    (if error-maybe
        (message "arietta: jsonrpc: %s - %s (%i)" -id
                 (plist-get error-maybe :message)
                 (plist-get error-maybe :code))
      ;; no error means success
      (pcase -id
        ("-arietta.active"
         (with-mutex arietta--pending-update
           (setf (alist-get 'active (slot-value connection 'data)) result)))
        ("-arietta.waiting"
         (with-mutex arietta--pending-update
           (setf (alist-get 'waiting (slot-value connection 'data)) result)))
        ("-arietta.stopped"
         (with-mutex arietta--pending-update
           (setf (alist-get 'stopped (slot-value connection 'data)) result)))
        ("-arietta.status" ;; (setf (slot-value connection 'status-data) result)
         (with-current-buffer (setq arietta--info-buffer (get-buffer-create "*arietta-info*"))
           (let ((inhibit-read-only t))
             (arietta-info-mode)
             (arietta--insert-info result)))
         (pop-to-buffer-same-window arietta--info-buffer))
        ("-arietta.globalStats" ;; (setf (slot-value connection 'global-stats) result)
         (with-current-buffer (setq arietta--info-buffer (get-buffer-create "*arietta-info*"))
           (let ((inhibit-read-only t))
             (erase-buffer)
             (arietta-info-mode)
             (insert "Aria2 daemon global stats:\n")
             (insert "  Total speed (U | D):     "
                     (arietta--size (plist-get result :uploadSpeed))
                     " | "
                     (arietta--size (plist-get result :downloadSpeed))
                     "\n")
             (insert "  Total downloads active:  " (plist-get result :numActive) "\n")
             (insert "-----\n\n")))
         (pop-to-buffer-same-window arietta--info-buffer))
        ("-arietta.getPeers"
         (when result
           (with-current-buffer (setq arietta--info-buffer (get-buffer-create "*arietta-info*"))
             (let ((inhibit-read-only t))
               (unless (eq major-mode 'arietta-info-mode)
                 (arietta-info-mode))
               (goto-char (point-max))
               (insert "\n\n")
               (if (featurep 'vtable)
                   (make-vtable :columns '((:name "IP")
                                           (:name "Port")
                                           (:name "D" :align 'right)
                                           (:name "U" :align 'right)
                                           (:name "Choking" :align 'right)
                                           (:name "Peer Choking" :align 'right)
                                           (:name "Seeder?" :align 'right))
                                :use-header-line nil
                                :objects result
                                :face 'default
                                :getter (lambda (obj col _tbl)
                                          (pcase col
                                            (0 (plist-get obj :ip))
                                            (1 (plist-get obj :port))
                                            (2 (file-size-human-readable
                                                (string-to-number (plist-get obj :downloadSpeed))))
                                            (3 (file-size-human-readable
                                                (string-to-number (plist-get obj :uploadSpeed))))
                                            (4 (plist-get obj :amChoking))
                                            (5 (plist-get obj :peerChoking))
                                            (6 (plist-get obj :seeder))))))))))
        ("-arietta.add" (message "arietta: sucessfully added"))
        ("-arietta.pause" (message "arietta: sucessfully paused"))
        ("-arietta.unpause" (message "arietta: sucessfully unpaused"))
        ("-arietta.forcePause" (message "arietta: sucessfully paused all downloads"))
        ("-arietta.pauseAll" (message "arietta: sucessfully paused all downloads"))
        ("-arietta.remove" (message "arietta: sucessfully removed"))
        ("-arietta.forceRemove" (message "arietta: sucessfully removed (forced)"))
        ("-arietta.purge" (message "arietta: sucessfully purged"))
        ;; ("-arietta.purgeAndRemove" (message "arietta: sucessfully purged and removed"))
        ("-arietta.saveSession" (message "arietta: session saved"))
        ("-arietta.shutdown" (message "arietta: shutdown succesfully"))
        ("-arietta.forceShutdown" (message "arietta: shutdown sucessfully (forced)"))
        (_ (arietta--notification-handler connection result))))))

;; unimplemented yet
(cl-defmethod arietta--notification-handler ((_connection arietta-rpc) data)
  ;; (message "arietta: notification recieved")
  (with-current-buffer (get-buffer-create "*arietta-debug*")
    (insert "\n\n======" (format "%s" data))))

(defun arietta--init-aria2-rpc ()
  "Initialize the aria2 RPC connection."
  (let ((connection (arietta-rpc
                     :websocket (websocket-open (concat (if arietta-use-tls "wss://" "ws://")
                                                        arietta-rpc-hostname ":"
                                                        (number-to-string arietta-rpc-port)
                                                        "/jsonrpc")
                                                :on-open (lambda (_) (message "arietta: sucessfully connected"))
                                                :on-close (lambda (_) (message "arietta: connection closed"))
                                                :on-message (lambda (_ws f)
                                                              (arietta--frame-handler arietta--rpc f))))))
    (if (websocket-openp (arietta--websocket connection))
        (prog1
            (setq arietta--rpc connection)
          (message "arietta: websocket connection opened"))
      (error "Couldn't open websocket connection"))))

(defun arietta--ensure ()
  "Ensure that the connection to aria2 is up."
  (unless (and arietta--rpc
               (arietta-websocket-p arietta--rpc))
    (message "arietta: connection down, attempting to create new connection...")
    (arietta--init-aria2-rpc)))

(defun arietta--update-downloads ()
  "Update all downloads, and the first `arietta-fetch-num' non-active."
  (arietta--ensure)
  (arietta-connection-send arietta--rpc
                           :id "-arietta.active"
                           :method 'aria2.tellActive)
  (arietta-connection-send arietta--rpc
                           :id "-arietta.waiting"
                           :method 'aria2.tellWaiting
                           :params (vector 0 arietta-fetch-num))
  (arietta-connection-send arietta--rpc
                           :id "-arietta.stopped"
                           :method 'aria2.tellStopped
                           :params (vector 0 arietta-fetch-num))
  ;; 3 connections = mutex will be unlocked and locked three times
  (mutex-lock arietta--pending-update)
  (mutex-lock arietta--pending-update)
  (mutex-lock arietta--pending-update))

(defun arietta--tabulated-entries ()
  "Return a list of aria2 downloads for `tabulated-list-mode'."
  (arietta--update-downloads)
  (let* ((data (arietta--data arietta--rpc))
         (active-list (cdr (assq 'active data)))
         (waiting-list (cdr (assq 'waiting data)))
         (stopped-list (cdr (assq 'stopped data)))
         (to-ret '()))
    ;; (dolist (item stopped-list)
    ;;   (if (not (plist-get item :bittorrent))
    ;;       (push (arietta--table-vector item) to-ret)))
    (dolist (item (append stopped-list waiting-list active-list))
      (push (arietta--table-vector item) to-ret))
    to-ret))

(defun arietta--divide (a b &optional percent)
  (if (equal b "0")
      "inf"
    (if percent
        (concat (calc-eval "round(($/$$)*100, 4)" nil a b) "%")
      (calc-eval "round($/$$, 4)" nil a b))))

;; (defun arietta--divide (a b &optional percent)
;;   (if (equal b "0")
;;       "inf"
;;     (if percent
;;         (concat (number-to-string (* 100 (/ (float (string-to-number a))
;;                                             (float (string-to-number b)))))
;;                 "%")
;;       (number-to-string (/ (float (string-to-number a))
;;                            (float (string-to-number b)))))))

(defsubst arietta--get-eta (down total)
  "Return human readable eta of download.
The download has a download speed of DOWN and a total size of TOTAL."
  (format-seconds "%yy,%dd,%hh,%mm,%z%ss" (string-to-number (arietta--divide total down))))

(defsubst arietta--propertize-status (string status)
  "Add faces to STRING according to STATUS."
  ;; current status faces: active, waiting, paused, complete, removed, error
  (propertize string 'face (intern (concat "arietta-" status))))

(defun arietta--table-vector (item)
  "Return table vector for one ITEM of `tabulated-list-mode'."
  (let* ((btl (plist-get item :bittorrent))
         (file (plist-get item :files))
         (status (plist-get item :status))
         (total-length (plist-get item :totalLength))
         (down-speed (plist-get item :downloadSpeed))
         (up-speed (plist-get item :uploadSpeed))
         (stop? (pcase status
                  ("complete" t)
                  ("paused" t))))
    (list (plist-get item :gid)
          (vector (list (if btl
                            (or (plist-get (plist-get btl :info) :name) "[empty]")
                          (file-name-nondirectory
                           (plist-get (car (plist-get item :files)) :path)))
                        'action #'arietta--list-click)
                  (arietta--size down-speed)
                  (arietta--size up-speed)
                  (arietta--size total-length)
                  (arietta--divide (plist-get item :completedLength) total-length t)
                  (cond ((and btl (equal "true" (plist-get item :seeder))) "seeding")
                        ((and file stop?)
                         "-")
                        (t (arietta--get-eta down-speed total-length)))
                  (arietta--propertize-status status status)
                  (if (and btl (not stop?))
                      (format "%s(%s)" (plist-get item :numSeeders) (plist-get item :connections))
                    "-")
                  (if btl
                      (arietta--divide (plist-get item :uploadLength) (plist-get item :completedLength))
                    "-")
                  (if btl "torrent" "file")))))

(defface arietta-active
  '((((class color) (background dark))  :foreground "dark sea green")
    (((class color) (background light)) :foreground "sea green"))
  "Face for active downloads."
  :group 'arietta)

(defface arietta-waiting
  '((((class color) (background dark))  :foreground "orchid1")
    (((class color) (background light)) :foreground "orchid4"))
  "Face for queued downloads."
  :group 'arietta)

(defface arietta-paused
  '((((class color) (background dark))  :foreground "wheat2")
    (((class color) (background light)) :foreground "wheat4"))
  "Face for paused downloads."
  :group 'arietta)

(defface arietta-complete
  '((((class color) (background dark))  :foreground "gray45")
    (((class color) (background light)) :foreground "gray60"))
  "Face for completed downloads."
  :group 'arietta)

(defface arietta-removed
  '((((class color) (background dark))  :foreground "burlywood1")
    (((class color) (background light)) :foreground "burlywood4")
    (((type graphic))                   :strike-through t))
  "Face for removed downloads."
  :group 'arietta)

(defface arietta-error
  '((((class color) (background dark))  :foreground "tomato1")
    (((class color) (background light)) :foreground "tomato4")
    (((type graphic))                   :strike-through t))
  "Face for removed downloads."
  :group 'arietta)

(defun arietta--list-click (p)
  "Handle a tabulated list click at point P."
  (mouse-set-point p)
  (arietta-connection-send arietta--rpc
                           :id "-arietta.status"
                           :method 'aria2.tellStatus
                           :params (vector (tabulated-list-get-id))))

(defsubst arietta--get-gid-from-info ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^gid: \\(.*\\)$" nil 'return-nil)
      (buffer-substring (match-beginning 1)
                        (match-end 1)))))

(defun arietta-refresh ()
  "Refresh the arietta list buffer or info buffer."
  (interactive)
  (cond ((eq (current-buffer) arietta--list-buffer)
         (revert-buffer)
         (when (not (timerp arietta--refresh-timer))
           (arietta--setup-timer)))
        ((eq (current-buffer) arietta--info-buffer)
         (when-let ((gid (arietta--get-gid-from-info)))
           (arietta-connection-send arietta--rpc
                                    :id "-arietta.status"
                                    :method 'aria2.tellStatus
                                    :params (vector gid))))))

(defun arietta--setup-timer ()
  (arietta--cancel-timer)
  (setq-default arietta--refresh-timer (run-with-timer 0 arietta-refresh-interval #'arietta-refresh))
  (add-hook 'kill-buffer-hook #'arietta--cancel-timer 0 t)
  (add-hook 'quit-window-hook #'arietta--cancel-timer 0 t))

(defun arietta--cancel-timer ()
  (when (timerp arietta--refresh-timer)
    (cancel-timer arietta--refresh-timer)
    (setq-default arietta--refresh-timer nil)))

(defun arietta--insert-info (item)
  "Insert info from ITEM into current buffer."
  (erase-buffer)
  (thunk-let* ((torrent-maybe (plist-get item :bittorrent))
               (torrent-name (or (plist-get (plist-get torrent-maybe :info) :name)
                                 "[none]"))
               (comment-maybe (plist-get torrent-maybe :comment))
               (seeding-maybe (plist-get item :seeder))
               (bitfield-maybe (arietta--progress-field (plist-get item :bitfield)))
               (mode-maybe (plist-get torrent-maybe :mode))
               (files-maybe (plist-get item :files))
               (name (or (file-name-nondirectory
                          (plist-get (car (plist-get item :files)) :path))
                         "[none]"))
               (status (plist-get item :status))
               (total-length (plist-get item :totalLength))
               (down-speed (plist-get item :downloadSpeed))
               (up-speed (plist-get item :uploadSpeed))
               (completed (plist-get item :completedLength))
               (dir (plist-get item :dir))
               (errcode-maybe (plist-get item :errorCode))
               (errmessage-maybe (plist-get item :errorMessage)))
    (if (not torrent-maybe)
        (insert "name: " name "\n" "type: file" "\n")
      (insert "name: " torrent-name "\n")
      (when comment-maybe (insert "comment: " comment-maybe  "\n"))
      (insert (format "type: bittorent (%s)\n" mode-maybe)))
    (insert "gid: " (plist-get item :gid) "\n")
    (insert "directory: " dir "\n"
            "status: " (arietta--propertize-status status status))
    (when (and torrent-maybe (equal status "active"))
      (insert (if (string= seeding-maybe "true") " (seeding)" " (leeching)")))
    (when (and errcode-maybe (not (string= errcode-maybe "0")))
      (insert (format " | %s %s (code: %s)"
                      (propertize "Error: " 'face 'arietta-error)
                      errmessage-maybe
                      errcode-maybe)))
    (when (equal status "active")
      (insert "\n"
              "progress: " (arietta--divide completed total-length t)
              "\n")
      (when torrent-maybe
        (insert "ETA: " (if (string= "false" seeding-maybe)
                            (arietta--get-eta down-speed
                                              total-length)
                          "-"))))
    (insert "\n\n")
    (insert "down: " (arietta--size down-speed) "\n")
    (insert "up: " (arietta--size up-speed) "\n")
    (insert "size: " (arietta--size total-length) "\n")
    (when torrent-maybe
      (insert "ratio: " (arietta--divide (plist-get item :uploadLength) (plist-get item :completedLength)) "\n")
      (insert "peers: " (format "%s(%s)"
                                (plist-get item :numSeeders)
                                (plist-get item :connections))
              "\n")
      (insert "pieces: " (if bitfield-maybe
                             (propertize bitfield-maybe 'face '(:box 1))
                           "-")
              "\n")
      (insert "announce: \n" )
      (dolist (u (plist-get torrent-maybe :announceList))
        (insert "  " (car u) "\n"))
      (when (equal mode-maybe "multi")
        (insert "\n\nFilelist:\n")
        (if (featurep 'vtable)
            (make-vtable :columns '((:name "File")
                                    (:name "P" :align right :width 8)
                                    (:name "S" :align right :width 8)
                                    (:name "Sel?" :align right :width 8))
                         :use-header-line nil
                         :separator-width 1
                         :objects files-maybe
                         :getter (lambda (obj col _tbl)
                                   (pcase col
                                     (0 (replace-regexp-in-string (concat "^" (regexp-quote dir) "/?") ""
                                                                  (plist-get obj :path)))
                                     (1 (arietta--divide (plist-get obj :completedLength)
                                                         (plist-get obj :length) t))
                                     (2 (file-size-human-readable (string-to-number
                                                                   (plist-get obj :length))))
                                     (3 (plist-get obj :selected)))))
          (dolist (obj files-maybe)
            (let* ((prog (arietta--divide (plist-get obj :completedLength)
                                          (plist-get obj :length) t))
                   (face (if (string= prog "100%") 'arietta-complete 'arietta-waiting)))
              (insert (propertize (format "  %s\n     - %s downloaded - %s | Selected: %s\n"
                                          (plist-get obj :path)
                                          prog
                                          (file-size-human-readable (string-to-number (plist-get obj :length)))
                                          (plist-get obj :selected))
                                  'face face))))))
      (arietta-connection-send arietta--rpc
                               :id "-arietta.getPeers"
                               :method 'aria2.getPeers
                               :params (vector (arietta--get-gid-from-info))))))

(defun arietta--progress-field (bitfield)
  "Convert BITFIELD to an ascii progress bar."
  (let* ((total (length bitfield))
         (division-length (/ total arietta-bitfield-divisions)))
    (cl-loop for sec in
             (cl-loop for div in (seq-partition bitfield division-length)
                      collect (string-to-number
                               (arietta--divide (cl-loop for c across div
                                                         sum (string-to-number (char-to-string c) 16))
                                                (* 15 division-length))))
             concat (cond ((<= sec 0.125) " ")
                          ((<= sec 0.25)  "▏")
                          ((<= sec 0.375) "▎")
                          ((<= sec 0.5)   "▍")
                          ((<= sec 0.625) "▌")
                          ((<= sec 0.75)  "▋")
                          ((< sec 1.0)    "▊")
                          ((= sec 1.0)    "▉")))))

(define-derived-mode arietta-info-mode fundamental-mode
  "Arietta-Info"
  :group 'arietta
  ;; (arietta--cancel-timer)
  ;; (erase-buffer)
  (setq buffer-read-only t)
  (pop-to-buffer-same-window (current-buffer)))

(define-derived-mode arietta-mode tabulated-list-mode
  "Arietta"
  :group 'arietta
  (arietta--ensure)
  (setf tabulated-list-format (vector
                               `("Name" ,(/ (window-width) 2) t)
                               '("D" 8 t :right-align t)
                               '("U" 8 t :right-align t)
                               '("Size" 10 t :right-align t)
                               '("Prog" 8 t :right-align t)
                               '("ETA" 8 t :right-align t)
                               '("Status" 9 t :right-align t)
                               '("C" 6 t :right-align t)
                               '("Ratio" 7 t :right-align t)
                               '("Type" 12 t :right-align t))
        tabulated-list-entries #'arietta--tabulated-entries
        tabulated-list-sort-key (cons "Prog" -1))
  (tabulated-list-init-header))

;; (define-derived-mode arietta-mode tabulated-list-mode
;;   "Arietta"
;;   :group 'arietta
;;   (arietta--ensure)
;;   (arietta--update-downloads)
;;   (let* ((data (arietta--data arietta--rpc))
;;          (inhibit-read-only t))
;;     (thread-last
;;       (make-taxy-magit-section
;;        :name "arietta"
;;        :taxys (list
;;                (make-taxy-magit-section :name "Active" :predicate (lambda (d) (equal (plist-get d :status) "active")))
;;                (make-taxy-magit-section :name "Paused" :predicate (lambda (d) (equal (plist-get d :status) "paused")))
;;                (make-taxy-magit-section :name "Completed" :predicate (lambda (d) (equal (plist-get d :status) "complete")))
;;                (make-taxy-magit-section :name "Other")))
;;       taxy-emptied
;;       (taxy-fill (append (cdr (assq 'active data))
;;                          (cdr (assq 'waiting data))
;;                          (cdr (assq 'stopped data))))
;;       taxy-magit-section-insert)))

(defun arietta-add-uri (uri)
  "Add URI to the list of queued downloads."
  (interactive "sAdd URI: ")
  (if (string= uri "")
      (user-error "I need a URI!"))
  (arietta--ensure)
  (arietta-connection-send arietta--rpc
                           :id "-arietta.add"
                           :method 'aria2.addUri
                           :params (vector (vector uri))))

(defsubst arietta-dirname-for-download (name)
  (if (tramp-tramp-file-p name)
      (tramp-file-name-localname (tramp-dissect-file-name name))
    (expand-file-name name)))

(defun arietta-add-uri-dir (udir uri)
  "Like callin `arietta-add-uri' on URI, but use UDIR as the download directory."
  (interactive (let ((default-directory (or arietta-prompt-directory
                                            default-directory)))
                 (list (read-directory-name "Choose Download Directory: ")
                       (read-string "Add URI: "))))
  (cond ((string= uri "") (user-error "I need a URI!"))
        ((not udir) (user-error "I need a directory!")))
  (arietta--ensure)
  (arietta-connection-send arietta--rpc
                           :id "-arietta.add"
                           :method 'aria2.addUri
                           :params (vector (vector uri)
                                           `((dir . ,(arietta-dirname-for-download udir))))))

(defun arietta-add-torrent (file)
  "Add .torrent file FILE to the list of queued downloads."
  (interactive "fTorrent file: ")
  (if (not (file-exists-p file))
      (user-error "Can't read file"))
  (arietta--ensure)
  (arietta-connection-send arietta--rpc
                           :id "-arietta.add"
                           :method 'aria2.addTorrent
                           :params (vector
                                    (base64-encode-string (with-temp-buffer
                                                            (insert-file-contents file)
                                                            (buffer-string))))))

(defun arietta-add-torrent-dir (udir file)
  "Like `arietta-add-torrent' on FILE, but use UDIR as the download directory."
  (interactive (let ((default-directory (or arietta-prompt-directory
                                            default-directory)))
                 (list (read-directory-name "Choose Download Directory: ")
                       (read-file-name "Torrent file: "))))
  (cond ((not (file-exists-p file)) (user-error "Can't read file"))
        ((not udir) (user-error "I need a directory!")))
  (arietta--ensure)
  (arietta-connection-send arietta--rpc
                           :id "-arietta.add"
                           :method 'aria2.addTorrent
                           :params (vector
                                    (base64-encode-string (with-temp-buffer
                                                            (insert-file-contents file)
                                                            (buffer-string)))
                                    []
                                    `((dir . ,(arietta-dirname-for-download udir))))))

(defmacro arietta--with-gid (&rest body)
  (declare (indent defun))
  `(when-let ((gid (tabulated-list-get-id)))
     ,@body))

(defun arietta-toggle-pause (&optional arg)
  "Toggle pause of download at point.

With prefix ARG, force pause the download instead if it is active,
which behaves like pausing normally except it doesn't perform actions
that may take time, like contacting bittorrent trackers to unregister."
  (interactive "P")
  (arietta--with-gid
    (pcase (aref (tabulated-list-get-entry) 6)
      ("active" (if arg
                    (arietta-connection-send arietta--rpc
                                             :id "-arietta.forcePause"
                                             :method 'aria2.forcePause
                                             :params (vector gid))
                  (arietta-connection-send arietta--rpc
                                           :id "-arietta.pause"
                                           :method 'aria2.pause
                                           :params (vector gid))))
      ("paused" (arietta-connection-send arietta--rpc
                                         :id "-arietta.unpause"
                                         :method 'aria2.unpause
                                         :params (vector gid)))
      (_ (user-error "Can't pause or unpause status: %s" (aref (tabulated-list-get-entry) 6))))))

(defun arietta-pause-all (&optional arg)
  "Pauses all downloads.

Equivalent to calling `aria2.pause' for all downloads.

With prefix ARG, force pause all downloads instead.
See `arietta-toggle-pause' for more information."
  (interactive "P")
  (arietta--with-gid
    (if arg
        (arietta-connection-send arietta--rpc
                                 :id "-arietta.pauseAll"
                                 :method 'aria2.pauseAll
                                 :params (vector gid))
      (arietta-connection-send arietta--rpc
                               :id "-arietta.pauseAll"
                               :method 'aria2.pauseAll
                               :params (vector gid)))))

(defun arietta-remove (&optional arg)
  "Remove an active download at point.

If the download is in progress, it is first stopped.
The status of the download becomes removed.

With prefix ARG, run `aria2.forceRemove' instead,
which behaves like `aria2.remove' except that it doesn't perform actions
that may take time, such as contacting BitTorrent trackers to unregister."
  (interactive "P")
  (if arg
      (arietta--with-gid
        (arietta-connection-send arietta--rpc
                                 :id "-arietta.forceRemove"
                                 :method 'aria2.forceRemove
                                 :params (vector gid)))
    (arietta--with-gid
      (arietta-connection-send arietta--rpc
                               :id "-arietta.remove"
                               :method 'aria2.remove
                               :params (vector gid)))))

(defun arietta-purge (&optional arg)
  "Clear a completed/error/removed download at point.

If called with prefix ARG, purge all completed/error/removed downloads."
  (interactive "P")
  (arietta--with-gid
    (if arg
        (arietta-connection-send arietta--rpc
                                 :id "-arietta.purge"
                                 :method 'aria2.purgeDownloadResult
                                 :params (vector gid))
      (arietta-connection-send arietta--rpc
                               :id "-arietta.remove"
                               :method 'aria2.removeDownloadResult
                               :params (vector gid)))))

;; TODO: ideally handle non-waiting queue downloads ourselves
(defun arietta-move-up ()
  "Move download at point up in the waiting queue."
  (interactive)
  (arietta--with-gid
    (arietta-connection-send arietta--rpc
                             :id "-arietta.move"
                             :method 'aria2.changePosition
                             :params (vector gid -1 "POS_CUR"))))

(defun arietta-move-down ()
  "Move download at point down in the waiting queue."
  (interactive)
  (arietta--with-gid
    (arietta-connection-send arietta--rpc
                             :id "-arietta.move"
                             :method 'aria2.changePosition
                             :params (vector gid +1 "POS_CUR"))))

(defun arietta-global-stats ()
  "Show some global stats for the aria2 daemon."
  (interactive)
  (arietta-connection-send arietta--rpc
                           :id "-arietta.globalStats"
                           :method 'aria2.getGlobalStat))

(defun arietta-save ()
  "Tell aria2 to save the current session to file specified by --save-session."
  (interactive)
  (arietta-connection-send arietta--rpc
                           :id "-arietta.saveSession"
                           :method 'aria2.saveSession))

(defun arietta-shutdown (&optional p)
  "Shutdown the aria2 server.

With prefix P, force shutdown."
  (interactive "P")
  (when (yes-or-no-p (if p "really force shutdown?" "really shutdown? "))
    (when (yes-or-no-p "save session before shutdown? ") (arietta-save))
    (if p
        (arietta-connection-send arietta--rpc
                                 :id "-arietta.forceShutdown"
                                 :method 'aria2.forceShutdown)
      (arietta-connection-send arietta--rpc
                               :id "-arietta.shutdown"
                               :method 'aria2.shutdown))))


(defvar arietta-mode-map (make-sparse-keymap))

(defun arietta--setup-keys ()
  "Setup keybindings for arietta."
  (set-keymap-parent arietta-mode-map tabulated-list-mode-map)
  (define-key arietta-mode-map "u" 'arietta-add-uri)
  (define-key arietta-mode-map "U" 'arietta-add-uri-dir)
  (define-key arietta-mode-map "t" 'arietta-add-torrent)
  (define-key arietta-mode-map "T" 'arietta-add-torrent-dir)
  (define-key arietta-mode-map "d" 'arietta-remove)
  (define-key arietta-mode-map "D" 'arietta-purge)
  (define-key arietta-mode-map "m" 'arietta-toggle-pause)
  (define-key arietta-mode-map "P" 'arietta-move-up)
  (define-key arietta-mode-map "N" 'arietta-move-down)
  (define-key arietta-mode-map "G" 'arietta-global-stats)
  (define-key arietta-mode-map "s" 'arietta-save)
  (define-key arietta-mode-map "g" 'arietta-refresh)

  (define-key arietta-info-mode-map "q" 'bury-buffer))

;;;###autoload
(defun arietta ()
  "Interface with the aria2 daemon."
  (interactive)
  (add-hook 'arietta-mode-hook #'arietta--setup-timer)
  (add-hook 'arietta-mode-hook #'arietta--setup-keys)
  (add-hook 'arietta-mode-hook #'hl-line-mode)
  (setq arietta--list-buffer (get-buffer-create "*arietta*"))
  (with-current-buffer arietta--list-buffer
    (arietta-mode)
    (pop-to-buffer-same-window (current-buffer))))

(provide 'arietta)

;;; arietta.el ends here
