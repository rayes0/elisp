;;; nnchan.el --- gnus backend for 4chan -*- lexical-binding: t; -*-
;; Version: 0.1
;; Package-Requires: ()

;; Author: rayes
;; URL: https://github.com/rayes0/elisp

;;; Commentary:
;; Thanks: https://github.com/dickmao/nntwitter/

;;; Code:

(require 'gnus)
(require 'gnus-start)
(require 'gnus-sum)
(require 'nnoo)
(require 'nnheader)
(require 'json)

(nnoo-declare nnchan)
(nnoo-define-basics nnchan)

(cl-defun nnchan-api (domain endpoint &key board (object 'plist) (array 'list))
  "Wrapper for the 4chan api."
  (with-current-buffer
      (url-retrieve-synchronously (concat "https://" domain ".4cdn.org/"
                                          (if board (file-name-as-directory board) "")
                                          endpoint))
    (goto-char url-http-end-of-headers)
    (json-parse-buffer :object-type object
                       :array-type array)))

(deffoo nnchan-open-server (_server &optional _definitions) t)
(deffoo nnchan-server-opened (&optional _server) t)
(deffoo nnchan-status-message (&optional _server) "")

(deffoo nnchan-retrieve-headers (articles &optional group _server _fetch-old)
  ;; (let ((remaining articles))
  (with-current-buffer nntp-server-buffer
    (erase-buffer)
    (dolist (article articles)
      (when-let ((post (nnchan--find-post article group)))
        (if (hash-table-p post)
            (nnheader-insert-nov (nnchan--make-header post))
          (nnheader-insert-nov (nnchan--make-header (cdr post) :reply-to (caar post) :reply-sub (cdar post))))))
    ;; (cl-loop for htb in (nnchan-api "a" "catalog.json" :board group :object 'hash-table)
    ;;          thereis (cl-loop for thread in (gethash "threads" htb)
    ;;                           if remaining
    ;;                           if (not (cl-loop for num in remaining
    ;;                                            if (eq num (gethash "no" thread))
    ;;                                            do
    ;;                                            (setf remaining (remove num remaining))
    ;;                                            (nnheader-insert-nov (nnchan--make-header thread))
    ;;                                            and return t))
    ;;                           do (cl-loop for reply in (gethash "last_replies" thread)
    ;;                                       do (cl-loop for num in remaining
    ;;                                                   if (eq num (gethash "no" reply))
    ;;                                                   do
    ;;                                                   (setf remaining (remove num remaining))
    ;;                                                   (nnheader-insert-nov (nnchan--make-header reply :reply-to thread))))
    ;;                           end
    ;;                           else return t))
    ;; )
    )
  'nov)

(cl-defun nnchan--make-header (thread &key reply-to reply-sub)
  "Make header from THREAD, a hash table of a JSON thread.
REPLY-TO is the id of the thread the post is a reply to if it is a reply.
REPLY-SUB is the subject of the thread the post is a reply to if it is a reply."
  (make-full-mail-header (gethash "no" thread)
                         (concat "Re: " reply-sub)
                         (gethash "name" thread)
                         (format-time-string "%a, %d %h %Y %T %z (%Z)" (gethash "time" thread))
                         (format "<%s@4chan.org>" (gethash "no" thread))
                         (when reply-to (format "<%s@4chan.org>" reply-to))))

(defvar nnchan-catalog nil
  "List of catalog data for each board, populated by `nnchan--find-post'.
Each element is of the form: (board last-updated-ts data)")

(deffoo nnchan-retrieve-groups (groups &optional _server)
  (cl-loop for group in groups
           for nums = (nnchan--min-max-articles group t)
           do (nnheader-insert "%s %s %s n\n" group (cdr nums) (car nums)))
  'active)

(deffoo nnchan-request-list (&optional _server)
  (gnus-message 5 "nnchan: fetching article list")
  (cl-loop for board in (plist-get (nnchan-api "a" "boards.json") :boards)
           for name = (plist-get board :board)
           for nums = (nnchan--min-max-articles name)
           concat (format "/%s/ %s %s n\n" name (cdr nums) (car nums))
           into text
           finally do (nnheader-insert text))
  t)

(cl-defun nnchan--catalog (board &key skip (update 30))
  "Return element of `nnchan--catalog' associated with BOARD.
Updates it if was updated more than UPDATE seconds ago (default: 15).
SKIP to skip updating."
  (let* ((cat (assoc board nnchan-catalog))
         (catalog (if cat
                      (cdr cat)
                    (push (list board (current-time) t) nnchan-catalog)
                    (setf skip t)
                    (cdr (assoc board nnchan-catalog))))
         (el (assoc board nnchan-catalog)))
    (when (and (not skip)
               (> (float-time (time-subtract (current-time) (car catalog))) update))
      (setf (nth 1 el) (current-time)
            (nth 2 el) (nnchan-api "a" "catalog.json"
                                   :board board
                                   :object 'hash-table)))
    el))

(deffoo nnchan-request-thread (header &optional group _server)
  (gnus-fetch-headers (if (not (mail-header-references header))
                          (nnchan--request-thread-1 (mail-header-id header) group)
                        (cl-loop with fetch = (mail-header-references header)
                                 while fetch
                                 do (setf fetch (with-current-buffer nntp-server-buffer
                                                  (erase-buffer)
                                                  (gnus-request-head
                                                   (gnus-extract-message-id-from-in-reply-to fetch)
                                                   group)
                                                  (message-fetch-field "references")))
                                 finally return (nnchan--request-thread-1 fetch group)))
                      nil t))

(defun nnchan--request-thread-1 (msg-id group)
  (cl-loop for post in (plist-get (nnchan-api "a" (concat "thread/"(substring msg-id 1 -11)".json")
                                              :board group)
                                  :posts)
           collect (plist-get post :no) into ids finally return ids))

;; (defun nnchan--find-post (id board)
;;   "Return a thread with ID in board BOARD.
;; Either a hash table or if the post is a reply, a list of form (parent . reply)."
;;   (cl-loop for htb in (nth 2 (nnchan--catalog board))
;;            thereis (cl-loop for thread in (gethash "threads" htb)
;;                             if (eq id (gethash "no" thread)) return thread
;;                             else thereis (cl-loop for reply in (gethash "last_replies" thread)
;;                                                   if (eq id (gethash "no" reply))
;;                                                   return (cons thread reply)))))

(defun nnchan--find-post (id board)
  "Return a post with ID in board BOARD.
Either a hash table or if the post is a reply, a list of form:

    ((parent-id . parent subject) . reply)."
  (cl-loop with list = (nth 2 (nnchan--catalog board))
           for htb in list
           for threads = (gethash "threads" htb)
           thereis (cl-loop for thread in threads
                            for op-id = (gethash "no" thread)
                            if (eq id op-id) return thread
                            else thereis (cl-loop for reply in (gethash "last_replies" thread)
                                                  if (eq id (gethash "no" reply))
                                                  return (cons (cons (if (equal 0 (gethash "resto" thread))
                                                                         op-id
                                                                       (gethash "resto" thread))
                                                                     (or (gethash "sub" thread)
                                                                         "[no subject]"))
                                                               reply)))
           ;; finally do (if (gnus-y-or-n-p (concat "couldn't find post no."
           ;;                                       (number-to-string id)
           ;;                                       " in recent replies. search all replies (slow)?"))
           ;;                (cl-loop for htb in list
           ;;                         for threads = (gethash "threads" htb)
           ;;                         thereis (cl-loop for thread in threads
           ;;                                          for op-id = (gethash "no" thread)
           ;;                                          thereis (cl-loop for reply-all
           ;;                                                           in (cdr (gethash "posts" (nnchan-api "a" (concat "thread/"
           ;;                                                                                                            (number-to-string op-id)
           ;;                                                                                                            ".json")
           ;;                                                                                                :board board
           ;;                                                                                                :object 'hash-table)))
           ;;                                                           if (eq id (gethash "no" reply-all))
           ;;                                                           return (cons (cons (gethash "resto" reply-all)
           ;;                                                                              (or (gethash "sub" thread)
           ;;                                                                                  "[no subject]"))
           ;;                                                                        reply-all)))))
           ))

(defun nnchan--min-max-articles (board &optional quick)
  "Return cons of (low high) article numbers for alist or string BOARD.
QUICK to use cached catalog instead of fetching."
  (cl-loop with low = most-positive-fixnum
           with high = most-negative-fixnum
           for htb in (nth 2 (nnchan--catalog board :skip quick :update 600))
           do (cl-loop for thread in (gethash "threads" htb)
                       for no = (gethash "no" thread)
                       if (< no low) do (setf low no)
                       if (> no high) do (setf high no))
           finally return (cons low high)))

(deffoo nnchan-request-article (article &optional group _server to-buffer)
  (with-current-buffer (or to-buffer nntp-server-buffer)
    (erase-buffer)
    (when-let* ((article (if (and (stringp article)
                                  (string-match-p "^<.*@4chan\\.org>$" article))
                             (string-to-number (substring article 1 -11))
                           article))
                (thread-reply-p (nnchan--find-post article group))
                (thread (if (listp thread-reply-p) (cdr thread-reply-p) thread-reply-p))
                (header (if (listp thread-reply-p)
                            (nnchan--make-header thread :reply-to (caar thread-reply-p) :reply-sub (cdar thread-reply-p))
                          (nnchan--make-header thread))))
      (insert "Newsgroups: " group "\n"
              "Subject: " (or (mail-header-subject header) "") "\n"
              "From: " (mail-header-from header) "\n"
              "Date: " (mail-header-date header) "\n"
              "Message-ID: " (mail-header-id header) "\n"
              (format "Link: <https://boards.4channel.org%sthread/%s/>\n" group article))
      (when-let ((references (mail-header-references header)))
        (insert "References: " references "\n"))
      (insert "\n")
      (mml-insert-multipart "alternative")
      (mml-insert-tag 'part 'type "text/html"
                      'disposition "inline"
                      'charset "utf-8")
      (save-excursion (mml-insert-tag '/part))
      (unless (listp thread-reply-p)
        (insert (format "<img src=\"https://i.4cdn.org/%s/%s%s\"/>"
                        group
                        (gethash "tim" thread)
                        (gethash "ext" thread))))
      (insert (gethash "com" thread) "\n")
      (if (mml-validate)
          (message-encode-message-body))
      (cons group article))))

(deffoo nnchan-request-group (group &optional _server _fast info)
  (let ((nums (nnchan--min-max-articles group)))
    (nnheader-insert (format "211 0 %s %s %s\n" (car nums) (cdr nums) group))
    (gnus-set-info gnus-newsgroup-name (or info (gnus-get-info gnus-newsgroup-name))))
  t)

(deffoo nnchan-request-close () t)
(deffoo nnchan-close-group (_group &optional _server) t)

(deffoo nnchan-request-post (&optional _server) nil)

(gnus-declare-backend "nnchan" 'post 'address)

(provide 'nnchan)

;;; nnchan.el ends here