;;; arietta.el --- simple interface for the aria2 daemon.  -*- lexical-binding: t; -*-

;; Author: rayes
;; Package-Requires: ((emacs "26.1") (json "1.5") (websocket "1.13"))
;; URL: https://github.com/rayes0/elisp

;;; Code:
;; LocalWords:  arietta rayes aria2c websocket URI tellStatus GID gid MATCHER
;; LocalWords:  STR ARG jsonrpc

(require 'websocket)
(require 'json)
(require 'cl-print)
(require 'tabulated-list)
(require 'thunk)

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
  "Token to use for connection."
  :type 'string
  :group 'arietta)

(defcustom arietta-refresh-interval 1.5
  "Refresh interval for when the arietta buffer is open."
  :type 'float
  :group 'arietta)

(defcustom arietta-fetch-num 100
  "Number of inactive (waiting or stopped) downloads to fetch.
A higher number will be slower if you have a lot of downloads.

All active downloads are always fetched."
  :type 'integer
  :group 'arietta)


(defvar arietta--rpc nil "Object containing the aria2c controller.")

(defclass arietta-rpc ()
  ((-websocket
    :initarg :websocket
    :accessor arietta--websocket)
   (data
    :initform ()
    :accessor arietta--data)
   (status-data
    :accessor arietta--status)
   (global-stats
    :accessor arietta--global-stats))
  "Controller and data for an aria2 RPC connection.")

(cl-defmethod arietta-connection-send ((connection arietta-rpc)
                                       &rest args
                                       &key
                                         _id
                                         method
                                         params)
  "Send ARGS as JSON to the aria2c endpoint CONNECTION via websocket."
  (arietta--ensure)
  (when method
    (plist-put args :method
               (cond ((keywordp method) (substring (symbol-name method) 1))
                     ((and method (symbolp method)) (symbol-name method)))))
  (if arietta-rpc-secret
      (plist-put args :params
                 (vconcat (vector (format "token:%s" arietta-rpc-secret)) params)))
  (let* ((message `(:jsonrpc "2.0" ,@args))
         (json (jsonrpc--json-encode message)))
    (websocket-send-text (arietta--websocket connection) json)))

;; from jsonrpc.el, to avoid loading the whole library
(defalias 'jsonrpc--json-encode
    (if (fboundp 'json-serialize)
        (lambda (object)
          (json-serialize object
                          :false-object :json-false
                          :null-object nil))
      (defvar json-false)
      (defvar json-null)
      (declare-function json-encode "json" (object))
      (lambda (object)
        (let ((json-false :json-false)
              (json-null nil))
          (json-encode object))))
  "Encode OBJECT into a JSON string.")

(cl-defmethod arietta-check-websocket ((connection arietta-rpc))
  "Return t if the websocket in CONNECTION is up."
  (let ((conn (arietta--websocket connection)))
    (if (websocket-openp conn)
        t
      nil)))

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
        (message "arietta: jsonrpc: %s - %s (%i)"
                 -id
                 (plist-get error-maybe :message)
                 (plist-get error-maybe :code))
      ;; no error means success
      (cond ((string= -id "-arietta.active") (setf (alist-get 'active (slot-value connection 'data))
                                                   result))
            ((string= -id "-arietta.waiting") (setf (alist-get 'waiting (slot-value connection 'data))
                                                    result))
            ((string= -id "-arietta.stopped") (setf (alist-get 'stopped (slot-value connection 'data))
                                                    result))
            ((string= -id "-arietta.status") (setf (slot-value connection 'status-data) result))
            ((string= -id "-arietta.globalStats") (setf (slot-value connection 'global-stats) result))
            ((string= -id "-arietta.add") (message "arietta: sucessfully added"))
            ((string= -id "-arietta.pause") (message "arietta: sucessfully paused"))
            ((string= -id "-arietta.unpause") (message "arietta: sucessfully unpaused"))
            ((string= -id "-arietta.remove") (message "arietta: sucessfully removed"))
            ((string= -id "-arietta.purge") (message "arietta: sucessfully purged"))
            ((string= -id "-arietta.purgeAndRemove") (message "arietta: sucessfully purged and removed"))
            ((string= -id "-arietta.saveSession") (message "arietta: session saved"))
            ((eq -id nil) (arietta--notification-handler connection result))))))

(cl-defmethod arietta--notification-handler ((connection arietta-rpc) data)
  (message "arietta: notification recieved")
  (setq plist-test data))


(defun arietta--init-aria2-rpc ()
  "Initialize the aria2 RPC connection."
  (let ((connection (arietta-rpc
                     :websocket (websocket-open (concat "ws://"
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
      (error "Websocket connection not open"))))

(defun arietta--ensure ()
  "Ensure that the connection to aria2 is up."
  (if (or (not arietta--rpc)
          (not (arietta-check-websocket arietta--rpc)))
      (progn (message "arietta: connection down, attempting to create new connection...")
             (arietta--init-aria2-rpc))))

(defun arietta--update-downloads ()
  "Update the active downloads, and the first 100 waiting."
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
                           :params (vector 0 arietta-fetch-num)))

(defun arietta--tabulated-entries ()
  "Return a list of aria2 downloads for `tabulated-list-mode'."
  (arietta--update-downloads)
  (let* ((data (arietta--data arietta--rpc))
         (active-list (cdr (assq 'active data)))
         (waiting-list (cdr (assq 'waiting data)))
         (stopped-list (cdr (assq 'stopped data)))
         (to-ret '()))
    (dolist (item stopped-list)
      (if (not (plist-get item :bittorrent))
          (push (arietta--table-vector item) to-ret)))
    (dolist (item (append active-list waiting-list))
      (push (arietta--table-vector item) to-ret))
    to-ret))

(defun arietta--table-vector (item)
  "Return table vector for one ITEM of `tabulated-list-mode'."
  (thunk-let ((btl (plist-get item :bittorrent))
              (status (plist-get item :status))
              (total-length (plist-get item :totalLength))
              (down-speed (plist-get item :downloadSpeed)))
    (cond (btl
           (list (plist-get item :gid)
                 (vector (list (plist-get (plist-get btl :info) :name)
                               'action #'arietta--list-click)
                         (arietta--human-readable-size down-speed)
                         (arietta--human-readable-size (plist-get item :uploadSpeed))
                         (arietta--human-readable-size total-length)
                         (or (arietta--divide (plist-get item :completedLength)
                                              total-length
                                              t)
                             "?")
                         (if (string= "false" (plist-get item :seeder))
                             (arietta--get-eta down-speed
                                               total-length)
                           "-")
                         (arietta--propertize-status status status)
                         (format "%s(%s)"
                                 (plist-get item :numSeeders)
                                 (plist-get item :connections))
                         "bittorrent")))
          ((plist-get item :files)
           (list (plist-get item :gid)
                 (vector (list (file-name-nondirectory
                                (plist-get (car (plist-get item :files))
                                           :path))
                               'action #'arietta--list-click)
                         (arietta--human-readable-size down-speed)
                         (arietta--human-readable-size (plist-get item :uploadSpeed))
                         (arietta--human-readable-size total-length)
                         (or (arietta--divide (plist-get item :completedLength)
                                              total-length
                                              t)
                             "?")
                         (if (string= "complete" (plist-get item :status))
                             "-"
                           (arietta--get-eta down-speed
                                             total-length))
                         (arietta--propertize-status status status)
                         " - "
                         "file"))))))

(defun arietta--divide (a b &optional percent)
  (if percent
      (concat (calc-eval "round(($/$$)*100, 4)" nil a b) "%")
    (calc-eval "round($/$$, 4)" nil a b)))

(defun arietta--get-eta (down total)
  "Return human readable eta of download with a
download speed of DOWN and a total size of TOTAL."
  (format-seconds "%yy,%dd,%hh,%mm,%z%ss" (string-to-number (arietta--divide total down))))

(defun arietta--human-readable-size (size-str)
  "Convert SIZE-STR to more friendly value."
  (let ((size (string-to-number size-str)))
    (cond ((= size 0) "0")
          ((> size 100000000000) (format "%1.2fT" (/ size 100000000000.0)))
          ((> size 1000000000) (format "%2.2fG" (/ size 1000000000.0)))
          ((> size 100000000) (format "%2.1fM" (/ size 1000000.0)))
          ((> size 1000000) (format "%2.2fM" (/ size 1000000.0)))
          ((> size 100000) (format "%2.2fk" (/ size 1000.0)))
          ((> size 1024) (format "%.2fk" (/ size 1000.0)))
          (t (format "%.2fB" size)))))

(defface arietta-active
    '((((class color) (background dark))
       :foreground "dark sea green")
      (((class color) (background light))
       :foreground "sea green"))
  "Face for active downloads."
  :group 'arietta)

(defface arietta-waiting
    '((((class color) (background dark))
       :foreground "orchid1")
      (((class color) (background light))
       :foreground "orchid4"))
  "Face for queued downloads."
  :group 'arietta)

(defface arietta-paused
    '((((class color) (background dark))
       :foreground "wheat2")
      (((class color) (background light))
       :foreground "wheat4"))
  "Face for paused downloads."
  :group 'arietta)

(defface arietta-complete
    '((((class color) (background dark))
       :foreground "gray45")
      (((class color) (background light))
       :foreground "gray60"))
  "Face for completed downloads."
  :group 'arietta)

(defface arietta-removed
    '((((class color) (background dark))
       :foreground "burlywood1")
      (((class color) (background light))
       :foreground "burlywood4")
      (((type graphic))
       :strike-through t))
  "Face for removed downloads."
  :group 'arietta)

(defface arietta-error
    '((((class color) (background dark))
       :foreground "tomato1")
      (((class color) (background light))
       :foreground "tomato4")
      (((type graphic))
       :strike-through t))
  "Face for removed downloads."
  :group 'arietta)

(defun arietta--propertize-status (string status)
  "Add faces to STRING according to STATUS."
  (cond ((string= status "active")   (propertize string 'face 'arietta-active))
        ((string= status "waiting")  (propertize string 'face 'arietta-waiting))
        ((string= status "paused")   (propertize string 'face 'arietta-paused))
        ((string= status "complete") (propertize string 'face 'arietta-complete))
        ((string= status "removed")  (propertize string 'face 'arietta-removed))
        ((string= status "error")    (propertize string 'face 'arietta-error))))

(defvar arietta--refresh-timer nil)
(defvar arietta--list-buffer nil)
(defvar arietta--info-buffer nil)

(defun arietta--list-click (p)
  "Handle a tabulated list click at point P."
  (mouse-set-point p)
  (arietta-connection-send arietta--rpc
                           :id "-arietta.status"
                           :method 'aria2.tellStatus
                           :params (vector (tabulated-list-get-id)))
  (setq arietta--info-buffer (get-buffer-create "*arietta-info*"))
  (with-current-buffer arietta--info-buffer
    (let ((inhibit-read-only t))
      (arietta-info-mode)
      (arietta--insert-info (arietta--status arietta--rpc)))))

(defun arietta--refresh ()
  (with-current-buffer arietta--list-buffer
    (revert-buffer)))

(defun arietta--setup-timer ()
  ;; (unless (and arietta--refresh-timer (eq major-mode 'arietta-mode))
  (setq-default arietta--refresh-timer (run-with-timer 0
                                                       arietta-refresh-interval
                                                       #'arietta--refresh))
  (add-hook 'kill-buffer-hook #'arietta--cancel-timer)
  (add-hook 'quit-window-hook #'arietta--cancel-timer))
;; )

(defun arietta--cancel-timer ()
  (when (timerp arietta--refresh-timer)
    (cancel-timer arietta--refresh-timer)
    (setq-default arietta--refresh-timer nil)))

(defun arietta--insert-info (item)
  "Insert info from ITEM into current buffer."
  (thunk-let* ((torrent-maybe (plist-get item :bittorrent))
               (torrent-name (plist-get (plist-get torrent-maybe :info)
                                        :name))
               (seeding-maybe (plist-get item :seeder))
               (name (plist-get item :name))
               (status (plist-get item :status))
               (total-length (plist-get item :totalLength))
               (down-speed (plist-get item :downloadSpeed))
               (up-speed (plist-get item :uploadSpeed))
               (completed (plist-get item :completedLength))
               (dir (plist-get item :dir)))
    (if torrent-maybe
        (insert "name: " torrent-name "\n" "type: bittorent")
      (insert "name: " name "\n" "type: file" "\n"))
    (insert dir "\n")
    (insert "status: "
            (arietta--propertize-status status status)
            (if (string= seeding-maybe "true") " (seeding)" " (leeching)")
            "\n")
    (insert "progress: " (arietta--divide completed total-length t) "\n")
    (when torrent-maybe
      (insert "ETA: " (if (string= "false" seeding-maybe)
                          (arietta--get-eta down-speed
                                            total-length)
                        "-")))
    (insert "\n")
    (insert "down: " (arietta--human-readable-size down-speed) "\n")
    (insert "up: " (arietta--human-readable-size up-speed) "\n")
    (insert "size: " (arietta--human-readable-size total-length) "\n")
    (when torrent-maybe
      (insert "connections: " (format "%s(%s)"
                                      (plist-get item :numSeeders)
                                      (plist-get item :connections))))))

(define-derived-mode arietta-info-mode fundamental-mode
  "Arietta-Info"
  :group 'arietta
  (arietta--cancel-timer)
  (erase-buffer)
  (setq buffer-read-only t)
  (pop-to-buffer-same-window (current-buffer)))

(define-derived-mode arietta-mode tabulated-list-mode
  "Arietta"
  :group 'arietta
  (arietta--ensure)
  (setf tabulated-list-format (vector
                               '("Name" 40 t)
                               '("D" 15 t)
                               '("U" 10 t)
                               '("Size" 6 t)
                               '("Progress" 12 t)
                               '("ETA" 8 t)
                               '("Status" 8 t)
                               '("C" 6 t)
                               '("Type" 15 t))
        tabulated-list-entries #'arietta--tabulated-entries
        tabulated-list-sort-key (cons "Progress" -1))
  (tabulated-list-init-header))

(defun arietta-add-uri (uri)
  "Add URI to the list of queued downloads."
  (interactive "sAdd URI: ")
  (if (string= uri "")
      (user-error "URI empty"))
  (arietta--ensure)
  (arietta-connection-send arietta--rpc
                           :id "-arietta.add"
                           :method 'aria2.addUri
                           :params (vector (vector uri))))

(defun arietta-add-uri-dir (udir uri)
  "Like callin `arietta-add-uri' on URI, but use UDIR as the download directory."
  (interactive "DChoose Download Directory: \nsAdd URI: ")
  (cond ((string= uri "") (user-error "URI empty"))
        ((not udir) (user-error "No specified directory")))
  (arietta--ensure)
  (arietta-connection-send arietta--rpc
                           :id "-arietta.add"
                           :method 'aria2.addUri
                           :params (vector (vector uri) `((dir . ,(expand-file-name udir))))))

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
  "Like calling `arietta-add-torrent' on FILE, but use UDIR as the download directory."
  (interactive "DChoose Download Directory: \nfTorrent file: ")
  (cond ((not (file-exists-p file)) (user-error "Can't read file"))
        ((not udir) (user-error "No specified directory")))
  (arietta--ensure)
  (arietta-connection-send arietta--rpc
                           :id "-arietta.add"
                           :method 'aria2.addTorrent
                           :params (vector
                                    (base64-encode-string (with-temp-buffer
                                                            (insert-file-contents file)
                                                            (buffer-string)))
                                    []
                                    `((dir . ,(expand-file-name udir))))))

(defun arietta-toggle-pause ()
  "Toggle pause of download at point."
  (interactive)
  (let ((gid (tabulated-list-get-id)))
    (when gid
      (let ((stat (aref (tabulated-list-get-entry) 6)))
        (cond ((string= stat "paused") (arietta-connection-send arietta--rpc
                                                                :id "-arietta.unpause"
                                                                :method 'aria2.unpause
                                                                :params (vector gid)))
              ((string= stat "active") (arietta-connection-send arietta--rpc
                                                                :id "-arietta.pause"
                                                                :method 'aria2.pause
                                                                :params (vector gid)))
              (t (user-error "Can't pause or unpause status: %s" stat)))))))

;; (defun arietta--check-statp (gid stat matcher)
;;   "Check if STAT for a download with gid GID matches stat given by MATCHER.
;; This is a wrapper over aria2.tellStatus."
;;   (arietta-connection-send arietta--rpc
;;                            :id "-arietta.putStatus"
;;                            :method 'aria2.tellStatus
;;                            :params (vector gid (vector stat)))
;;   (unless (not (string= (plist-get (arietta--last-check-stat arietta--rpc) :status) matcher))
;;     t))

(defun arietta-remove ()
  "Remove an active download at point."
  (interactive)
  (let ((gid (tabulated-list-get-id)))
    (when gid
      (arietta-connection-send arietta--rpc
                               :id "-arietta.remove"
                               :method 'aria2.remove
                               :params (vector gid)))))

(defun arietta-purge (&optional arg)
  "Clear a completed/error/removed download at point.

If called with prefix ARG, remove download result as well."
  (interactive "P")
  (let ((gid (tabulated-list-get-id)))
    (when gid
      (if arg
          (arietta-connection-send arietta--rpc
                                   :id "-arietta.purgeAndRemove"
                                   :method 'aria2.removeDownloadResult
                                   :params (vector gid))
        (arietta-connection-send arietta--rpc
                                 :id "-arietta.purge"
                                 :method 'aria2.purgeDownloadResult
                                 :params (vector gid))))))

;; TODO: ideally handle non-waiting queue downloads ourselves
(defun arietta-move-up ()
  "Move download at point up relative to it's current position in the waiting queue."
  (interactive)
  (let ((gid (tabulated-list-get-id)))
    (when gid
      (arietta-connection-send arietta--rpc
                               :id "-arietta.move"
                               :method 'aria2.changePosition
                               :params (vector gid -1 "POS_CUR")))))

(defun arietta-move-down ()
  "Move download at point up relative to it's current position in the waiting queue."
  (interactive)
  (let ((gid (tabulated-list-get-id)))
    (when gid
      (arietta-connection-send arietta--rpc
                               :id "-arietta.move"
                               :method 'aria2.changePosition
                               :params (vector gid +1 "POS_CUR")))))

(defun arietta-global-stats ()
  "Show some global stats for the aria2 daemon."
  (interactive)
  (arietta-connection-send arietta--rpc
                           :id "-arietta.globalStats"
                           :method 'aria2.getGlobalStat)
  (setq arietta--info-buffer (get-buffer-create "*arietta-info*"))
  (with-current-buffer arietta--info-buffer
    (let ((inhibit-read-only t)
          (info (arietta--global-stats arietta--rpc)))
      (arietta-info-mode)
      (insert "Aria2 daemon global stats:\n")
      (insert "  Total speed (U | D):     "
              (arietta--human-readable-size (plist-get info :uploadSpeed))
              " | "
              (arietta--human-readable-size (plist-get info :downloadSpeed))
              "\n")
      (insert "  Total downloads active:  " (plist-get info :numActive) "\n")
      (insert "-----\n\n")))
  (pop-to-buffer-same-window arietta--info-buffer))

(defun arietta-save ()
  "Tell aria2 to save the current session to file specified by --save-session."
  (interactive)
  (arietta-connection-send arietta--rpc
                           :id "-arietta.saveSession"
                           :method 'aria2.saveSession))


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

  (define-key arietta-info-mode-map "q" 'bury-buffer))

;;;###autoload
(defun arietta ()
  "Interface with the aria2 daemon."
  (interactive)
  (add-hook 'arietta-mode-hook #'arietta--setup-timer)
  (add-hook 'arietta-mode-hook #'arietta--setup-keys)
  (setq arietta--list-buffer (get-buffer-create "*arietta*"))
  (with-current-buffer arietta--list-buffer
    (arietta-mode)
    (pop-to-buffer-same-window (current-buffer))))

(provide 'arietta)

;;; arietta.el ends here
