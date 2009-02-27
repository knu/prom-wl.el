;;; -*- Emacs-Lisp -*-
;;; procmail reader for Wanderlust on Emacsen.
;;; $Id: prom-wl.el,v 2.28.2.5 2001/09/08 08:09:46 murata Exp $
;;; by Masahiro MURATA <muse@ba2.so-net.ne.jp>

(defconst prom-wl-version "Prom-WL 2.7.0")

;; !!! this version is required Wanderlust 2.7.0 or later!!!

;;; @ Document
;;; 

;;; @ require
;;;

(require 'wl)
(eval-when-compile
  (require 'elmo-imap4)
  (require 'wl-util))

;;; @ Customization:
;;;

;;; common at Prom-Mew

(defvar proc-log-list nil
  "*Logfile list of procmailrc.
If file prefix is '%', log file exists on imap4 server.

  (setq proc-log-list \'(\"~/Mail/from-log\" \"%Mail/ml-log\"))
")

(defvar proc-keep-log nil
  "*Keeping logfile. If nil, not keeping.

  (setq proc-keep-log \"~/Mail/listlog\")
")

(defvar proc-lock-file "~/Mail/.lock"
  "*Global lockfile of procmail.")

(defvar proc-folder-regexp nil
  "*Optional folder regexp for checking procmail logfile.

If you use dmail in ~/.procmailrc, try:

(setq proc-folder-regexp \"^  Folder:[ ]*dmail[ ]+\\\\+\\\\([^ \\t]+\\\\)\")
")

(defvar prom-use-lockfile t
  "*If non-nil, use lockfile program of procmail package.")

(defvar prom-prog-lockfile "lockfile")

(defvar prom-prog-lockfile-arg-list '("-2" "-r4" "-l10")
  "*Argument list for lockfile program")

(defvar prom-lock-at-exist-log t
  "If nil, lock always. non-nil, lock at exists log file.")

;;; Prom-WL specific

(defvar prom-wl-lock-optional-method t
  "*If non-nil, lock on `prom-wl-get-new-mail-optional-method'.")

(defvar prom-wl-get-new-mail-optional-method nil
  "*Optional method called at prom-get-new-mail.

  (setq prom-wl-get-new-mail-optional-method \'prom-wl-check-list-folders)
")

(defvar prom-wl-check-folders nil
  "*Folders list of check unread folder.

   (setq prom-wl-check-folders \'(\"+inbox\" \"+private\" \"%#mh/ml/wl\"))
")

(defvar prom-wl-imap4-read-log-function 'prom-wl-imap4-read-log-by-rename)

(defvar prom-wl-folder-prefix-alist
  '((imap4 . "%#mh/")
    (nil   . "+"))	;; local file
  "*Prefix of checking folder.")

(defvar prom-wl-ignore-folder "^\\+\\(draft\\|trash\\|outbox\\)$")

(defvar prom-wl-xheader nil)

(defvar prom-wl-entity-alias nil)

(defvar prom-wl-auto-select-folder nil
  "*If non-nil, select folder automatically when exists unread folder.")

(defvar prom-wl-keep-log-max-size nil
  "*Max size of file that specified `proc-keep-log'.")

(defvar prom-wl-ignore-not-found-entity nil
  "*If non-nil, ignore `not found entity' error.")

(defvar prom-wl-lock-localdir-msgdb-create nil)

(defvar prom-wl-set-promrc-log-function 'prom-wl-set-promrc-log
  "*A function for parsing procmail logfile.")

;;; Hooks

(defvar prom-wl-get-new-mail-pre-hook nil)
(defvar prom-wl-get-new-mail-hook nil)
(defvar prom-wl-get-proc-log-hook nil)
(defvar prom-wl-load-hook nil)
(defvar prom-wl-hook nil)

;;; @ Global Variables:
;;;

(defvar prom-wl-init nil)
(defvar prom-wl-check-entity-name "new")
(defvar prom-check-entity nil)
(defvar prom-check-entity-list nil)
(defvar prom-lock-status nil)
(defvar prom-wl-previous-window-config nil)

(defconst prom-buffer-tmp " *prom-wl tmp*")


;;; @ code
;;;

(defvar prom-wl-generate-mailer-string-function nil
  "(setq prom-wl-generate-mailer-string-function wl-generate-user-agent-string)")

(defun prom-wl-generate-xheader-string ()
  (concat (funcall prom-wl-generate-mailer-string-function)
	  "\n"
	  (prom-wl-xheader-func)))

(defun prom-wl-xheader-func ()
  (concat "X-Prom-WL: " prom-wl-version " (procmail reader for Wanderlust)"))

(defmacro prom-with-temp-buffer (&rest forms)
  (` (let ((tmp-buf (get-buffer-create prom-buffer-tmp)))
       (unwind-protect
	   (save-excursion
	     (set-buffer tmp-buf)
	     (erase-buffer)
	     (,@ forms))
	 (and (get-buffer tmp-buf)
	      (kill-buffer tmp-buf))))))

(defmacro prom-wl-plugged-p (folder)
  (` (elmo-folder-plugged-p (wl-folder-get-elmo-folder (, folder)))))

(defsubst prom-wl-folder-type (folder)
  (let ((type (elmo-folder-type folder)))
    (unless (and type (string-match "^\\(.:\\)?/" folder))
      type)))

;;; for wl-folder mode
;;;

(defmacro prom-wl-folder-unread-regex (group)
  (` (format "^[ ]*%s+:[0-9\\*-]+/[^0\\*][0-9]*/[0-9\\*-]+$"
	     (if (, group)
		 "."
	       "[^[:]"))))

(defun prom-wl-folder-next-unread (&optional nogroup)
  "move cursor to the next unread folder."
  (interactive "P")
  (let ((start-point (point))
	folder)
    (end-of-line)
    (setq folder
	  (catch 'done
	    (while (re-search-forward
		    (prom-wl-folder-unread-regex (not nogroup)) nil t)
	      (save-excursion
		(beginning-of-line)
		(if (looking-at "^[ ]*\\[[\\+-]\\]\\(.+\\)\n")
		    (throw 'done (wl-match-buffer 1))
		  (setq folder (wl-folder-get-folder-name-by-id
				(get-text-property (point) 'wl-folder-entity-id)))
		  (if (string-match prom-wl-ignore-folder folder)
		      nil
		    (throw 'done folder)))))
	    (throw 'done nil)))
    (if folder
	(progn
	  (beginning-of-line)
	  folder)
      (goto-char start-point)
      (message "no more unread folder")
      nil)))

(defun prom-wl-folder-jump-folders (folders)
  (let (point select-folder)
    (save-excursion
      (setq point
	    (catch 'found
	      (goto-char (point-min))
	      (while (setq select-folder (prom-wl-folder-next-unread t))
		(if (wl-string-member select-folder folders)
		    (throw 'found (point)))))))
    (if point
	(goto-char point))))

(defun prom-wl-folder-open-folder2 (folders)
  (let* (id path path-list)
    (while folders
      (when (setq id (wl-folder-get-entity-id (car folders)))
	(setq path (delete id (wl-folder-get-path wl-folder-entity id)))
	(when (not (member path path-list))
	  (wl-append path-list (list path))
	  (wl-folder-open-folder-sub path)))
      (setq folders (cdr folders)))))

;;; for prom-wl
;;;

(defun prom-wl-setup ()
  (unless prom-wl-init
    (when prom-wl-xheader
      (or prom-wl-generate-mailer-string-function
	  (setq prom-wl-generate-mailer-string-function
		wl-generate-mailer-string-function))
      (setq wl-generate-mailer-string-function 'prom-wl-generate-xheader-string))

    (when prom-wl-lock-localdir-msgdb-create
      ;; lock when create summary of localdir
      (defadvice elmo-localdir-msgdb-create-as-numlist (around lock activate)
	(unless (cond (prom-use-lockfile
		       (prom-wait-lock proc-lock-file))
		      (t
		       (prom-make-lock proc-lock-file)))
	  (error "%s: lock failed" proc-lock-file))
	ad-do-it
	(let ((prom-lock-status 'local))
	  (prom-unlock proc-lock-file))))

    (setq prom-wl-init t)))

(defun prom-wl (&optional arg)
  (interactive "P")
  (if (not (memq major-mode '(wl-folder-mode wl-summary-mode)))
      (setq prom-wl-previous-window-config (current-window-configuration)))
  (let ((wl-auto-check-folder-name 'none))
    (wl))
  (let ((prom-wl-auto-select-folder
	 (if arg
	     nil
	   prom-wl-auto-select-folder)))
    (prom-wl-check-new-mail)
    (run-hooks 'prom-wl-hook)))

(defun prom-wl-exit ()
  (interactive)
  (wl-exit)
  (if (get-buffer prom-buffer-tmp)
      (kill-buffer prom-buffer-tmp))
  (if prom-wl-previous-window-config
      (set-window-configuration prom-wl-previous-window-config)))

(defun prom-wl-suspend ()
  (interactive)
  (wl-folder-suspend)
  (if prom-wl-previous-window-config
      (set-window-configuration prom-wl-previous-window-config)))

(defmacro prom-call-optional-method (lock-mode)
  (` (if (eq (, lock-mode) prom-wl-lock-optional-method)
	 (and prom-wl-get-new-mail-optional-method
	      (funcall prom-wl-get-new-mail-optional-method arg)))))

(defun prom-wl-check-new-mail (&optional arg)
  "Check New mail, from procmail log files.
If arg is non-nil, check unread folders."
  (let (status select-folder)
    (prom-wl-setup)
    (setq prom-check-entity nil)
    (setq prom-check-entity-list nil)
    (run-hooks 'prom-wl-get-new-mail-pre-hook)
    ;;
    (prom-call-optional-method nil)	;; call at non-lock-mode
    ;; [[ Lock ]]
    (setq status (prom-lock proc-lock-file))
    (cond ((eq status 'error)
	   ;; lock failed
	   (message "lock file `%s' exists! Please wait a minute."
		    proc-lock-file)
	   (ding)
	   (sit-for 1))
	  (status
	   ;; lock successed or log on network
	   (prom-call-optional-method t) ;; call at lock-mode
	   (prom-wl-get-proc-log)
	   ;; [[ Unlock ]]
	   (prom-unlock proc-lock-file)))
    (run-hooks 'prom-wl-get-new-mail-hook)
    (when prom-check-entity-list
      (let ((entity-list prom-check-entity-list)
	    entity alias)
	(while entity-list
	  (setq entity (car entity-list))
	  (when (setq alias (assoc entity prom-wl-entity-alias))
	    (setq prom-check-entity-list
		  (delete entity prom-check-entity-list))
	    (setq entity (cdr alias))
	    (wl-append prom-check-entity-list (list entity)))
	  (if (wl-folder-search-entity-by-name
	       entity
	       wl-folder-entity)
	      nil
	    (unless prom-wl-ignore-not-found-entity
	      (message "%s: not found entity. press any key." entity)
	      (ding)
	      (wl-read-event-char))
	    (setq prom-check-entity-list
		  (delete entity prom-check-entity-list)))
	  (setq entity-list (cdr entity-list))))
      (setq prom-check-entity
	    (list prom-wl-check-entity-name 'group prom-check-entity-list))
      (wl-folder-check-entity prom-check-entity)
      (let ((entity-list prom-check-entity-list)
	    unread-folders)
	(while entity-list
	  (let* ((nums (wl-folder-get-entity-info (car entity-list)))
		 (unread (or (and (nth 0 nums)(nth 1 nums)
				  (+ (nth 0 nums)(nth 1 nums)))
			     0)))
	    (if (> unread 0)
		(wl-append unread-folders
			   (list (wl-folder-search-entity-by-name
				  (car entity-list) wl-folder-entity
				  'folder))))
	    (setq entity-list (cdr entity-list))))
	(prom-wl-folder-open-folder2 unread-folders))
      (setq select-folder
	    (prom-wl-folder-jump-folders prom-check-entity-list)))
    (if (and prom-wl-auto-select-folder
	     select-folder)
	(wl-folder-jump-to-current-entity))))

;;; get-new-mail optional-method

(defun prom-wl-check-list-folders (&optional arg)
  (let ((list prom-wl-check-folders))
    (while list
      (or (member (car list) prom-check-entity-list)
	  (wl-append prom-check-entity-list (list (car list))))
      (setq list (cdr list)))))

(defsubst prom-wl-get-folder-prefix (type)
  (cdr (assoc type prom-wl-folder-prefix-alist)))

(defun prom-wl-get-proc-log ()
  (prom-with-temp-buffer
   (let ((log-list proc-log-list)
	 proc-log type prefix)
     (while log-list
       (setq proc-log (car log-list))
       (setq type (prom-wl-folder-type proc-log))
       (cond
	((not type)		;; local logfile
	 (when (file-exists-p proc-log)
	   (goto-char (point-max))
	   (as-binary-input-file (insert-file-contents proc-log))
	   (delete-file proc-log)))
	((eq type 'imap4)	;; logfile on imap4 server
	 (when (prom-wl-plugged-p proc-log)
	   (let (contents)
	     (message "checking log of imap server...")
	     (when (setq contents
			 (funcall prom-wl-imap4-read-log-function proc-log))
	       (goto-char (point-max))
	       (as-binary-input-file (insert contents)))
	     (message ""))))
	(t
	 (error "%s: not supported type." proc-log)))
       (when (not (zerop (buffer-size)))
	 (unless (setq prefix (prom-wl-get-folder-prefix type))
	   (error "no prefix of type: %s" type))
	 (prom-wl-append-keep-log)
	 (funcall prom-wl-set-promrc-log-function prefix)
	 (erase-buffer))
       (setq log-list (cdr log-list)))
     (run-hooks 'prom-wl-get-proc-log-hook))))

(defun prom-wl-append-keep-log ()
  (when proc-keep-log
    (let ((log-size (nth 7 (file-attributes proc-keep-log))))
      (if (and prom-wl-keep-log-max-size log-size
	       (> log-size prom-wl-keep-log-max-size))
	  (rename-file proc-keep-log (concat proc-keep-log ".bak") t))
      (if (file-writable-p proc-keep-log)
	  (save-excursion
	    (write-region (point-min) (point-max) proc-keep-log t 'no-msg))
	(message "not writable file! `%s'" proc-keep-log)))))

;;; get log by imap4

(defvar prom-wl-imap4-close t)

(defvar prom-wl-imap4-read-old-folder t)

(defvar prom-wl-dummy-from "From ????(added-automatically-by-Prom-WL)")

(defmacro prom-wl-imap4-log2tmp (log)
  (` (format "%s.prom-wl" (, log))))

;; for old elmo-imap4.el

(defun prom-wl-imap4-filter (string)
  "*Erase a prefix of imap4."
  (elmo-replace-in-string
   (elmo-replace-in-string
    string "\n?\n)" "")
   "\* 1 FETCH (BODY\[\] \{[0-9]+\}" prom-wl-dummy-from))

;; for new elmo-imap4.el

(defun prom-wl-imap4-read-log (proc-log)
  (save-excursion
    (let* ((folder (wl-folder-get-elmo-folder proc-log))
	   (session (elmo-imap4-get-session folder))
	   exists response)
      (condition-case ()
	  (setq exists (elmo-imap4-response-value
			(elmo-imap4-session-select-mailbox
			 session
			 (elmo-imap4-folder-mailbox-internal folder))
			'exists))
	(error nil))
      (with-current-buffer (elmo-network-session-buffer session)
	(setq elmo-imap4-fetch-callback nil)
	(setq elmo-imap4-fetch-callback-data nil))
      (when (and exists (> exists 0))
	(and
	 (elmo-imap4-response-value
	  (elmo-imap4-send-command-wait session "store 1 +flags (\\Deleted)")
	  'fetch)
	 (setq response
	       (concat
		prom-wl-dummy-from "\n"
		(elmo-delete-cr
		 (elmo-imap4-response-bodydetail-text
		  (elmo-imap4-response-value-all
		   (elmo-imap4-send-command-wait session "fetch 1 body.peek[]")
		   'fetch)))))
	 (elmo-imap4-response-value
	  (elmo-imap4-send-command-wait session "expunge")
	  'expunge)))
      (when prom-wl-imap4-close
	(elmo-network-close-session session))
      response)))

(defun prom-wl-imap4-read-log-by-rename (proc-log)
  (save-excursion
    (let* ((spec (wl-folder-get-elmo-folder proc-log))
	   (folder (elmo-imap4-folder-mailbox-internal spec))
	   (temp-folder (prom-wl-imap4-log2tmp folder))
	   (session (elmo-imap4-get-session spec))
	   response old-contents retval)
      (when prom-wl-imap4-read-old-folder
	(setq old-contents
	      (prom-wl-imap4-read-and-delete temp-folder session t)))
      (setq response
	    (elmo-imap4-read-response
	     session
	     (elmo-imap4-send-command
	      session (format "rename %s %s" folder temp-folder))))
      (if (elmo-imap4-response-ok-p response)
	  ;; rename success
	  (progn
	    (setq retval (prom-wl-imap4-read-and-delete temp-folder session))
	    (if old-contents
		(concat old-contents retval)
	      retval))
	;; rename fail
	(if old-contents
	    old-contents)))))

(defun prom-wl-imap4-read-and-delete (folder session
					     &optional select-noerror)
  (let (exists response)
    (condition-case ()
	(setq exists (elmo-imap4-response-value
		      (elmo-imap4-session-select-mailbox session folder)
		      'exists))
      (error nil))
    (with-current-buffer (elmo-network-session-buffer session)
      (setq elmo-imap4-fetch-callback nil)
      (setq elmo-imap4-fetch-callback-data nil))
    (when (and exists (> exists 0))
      (and
       (elmo-imap4-response-value
	(elmo-imap4-send-command-wait session "store 1 +flags (\\Deleted)")
	'fetch)
       (setq response
	     (concat
	      prom-wl-dummy-from "\n"
	      (elmo-delete-cr
	       (elmo-imap4-response-bodydetail-text
		(elmo-imap4-response-value-all
		 (elmo-imap4-send-command-wait session "fetch 1 body.peek[]")
		 'fetch)))))
       (elmo-imap4-send-command-wait session "close")
       (elmo-imap4-send-command-wait session (concat "delete " folder))))
    response))

;;; end of imap4

(defun prom-wl-set-promrc-log (prefix)
  (save-excursion
    (goto-char (point-min))
    (let (folder path)
      (while
	  (cond
	   ((and proc-folder-regexp
		 (re-search-forward proc-folder-regexp nil t)
		 (setq folder (concat prefix (elmo-match-buffer 1))))
	    t)
	   ((and (re-search-forward "^  Folder: \\(.+\\)/\\([0-9]+\\)" nil t)
		 (setq path (elmo-match-buffer 1))
		 (progn
		   (cond ((string-match "^[%-+=]" path)
			  (setq folder path))
			 ;; Absolute path (include DriveLetter)
			 ((string-match "^\\(.:\\)?/" path)
			  (cond
			   ((string-match (concat "^" (expand-file-name elmo-localdir-folder-path) "/*") path)
			    (setq folder
				  (concat prefix (substring path (match-end 0)))))
			   (t
			    (setq folder path))))
			 ;; mail folder
			 (t
			  (setq folder (concat prefix path))))))
	    t))
	(prom-wl-add-check-entity-list folder)))))

(defun prom-wl-add-check-entity-list (folder)
  (if (not (member folder prom-check-entity-list))
      (wl-append prom-check-entity-list (list folder))))

;;; lock

(defun prom-lock (lockfile)
  (setq prom-lock-status 'local)	;; lock always,
					;; if prom-lock-at-exist-log is nil.
  (if prom-lock-at-exist-log
      (setq prom-lock-status
	    (let ((log-list proc-log-list)
		  (locktype nil)
		  proc-log type)
	      (catch 'exist
		(while log-list
		  (setq proc-log (car log-list))
		  (setq type (prom-wl-folder-type proc-log))
		  (cond ((not type) ;; local file
			 (if (file-exists-p proc-log)
			     (throw 'exist 'local)))
			(t
			 (setq locktype 'network)))
		  (setq log-list (cdr log-list)))
		locktype))))
  (when (and (eq prom-lock-status 'local)
	     (not (cond (prom-use-lockfile
			 (prom-wait-lock lockfile))
			(t
			 (prom-make-lock lockfile)))))
    (setq prom-lock-status 'error))
  prom-lock-status)

(defun prom-unlock (lockfile)
  (if (and (eq prom-lock-status 'local)
	   (file-exists-p lockfile))
      (delete-file lockfile)))

(defun prom-make-lock (lockfile)
  (let ((status (call-process "ln" nil nil nil
			      "-s" "prom-wl" (expand-file-name lockfile))))
    (if (= 0 status)
	t
      (message "lock file exists!!")
      nil)))

(defun prom-wait-lock (lockfile)
  (message "Now locking..." lockfile)
  (let ((status (apply (function call-process)
		       prom-prog-lockfile nil nil nil
		       (append
			prom-prog-lockfile-arg-list
			(list (expand-file-name lockfile))))))
    (if (= 0 status)
	(progn
	  (message "")
	  t)
      (message "lock failed!!")
      nil)))

;;; @ end
;;;

(run-hooks 'prom-wl-load-hook)

(provide 'prom-wl)

;;; Local variables:
;;; mode: outline-minor
;;; outline-regexp: ";;; @+\\|(......"
;;; End:
