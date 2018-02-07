;;; Mail stuff

;;(load-library "supercite")
;;(add-hook 'mail-yank-hooks 'sc-cite-original)

(defun my-mail-mode-hook ()
  (let ((mail-mode-font-lock-keywords
	(append
	 message-font-lock-keywords
	 (list
	  (cons (concat "^" (make-string 80 ?.) "\\(.*\\)$")
		'font-lock-comment-face)
	  ))))
    (auto-fill-mode 1)
    (setq fill-column 78)
    (setq font-lock-keywords mail-mode-font-lock-keywords)
    (turn-on-font-lock)))

(add-hook 'mail-mode-hook 'my-mail-mode-hook)

(setq mail-self-blind t
      mail-default-reply-to "ktaylor@pobox.com"
      mail-signature t)

(setq send-mail-function
      (function
       (lambda ()
	(interactive)
	;;(ispell-message)
	(if (y-or-n-p "Send message? ")
	    (sendmail-send-it)
	    (error "Send Aborted")))))

;;; VM
(autoload 'vm "vm" "Start VM on your primary inbox." t)
(autoload 'vm-visit-folder "vm" "Start VM on an arbitrary folder." t)
(autoload 'vm-visit-virtual-folder "vm" "Visit a VM virtual folder." t)
(autoload 'vm-mode "vm" "Run VM major mode on a buffer" t)
(autoload 'vm-mail "vm" "Send a mail message using VM." t)
(autoload 'vm-submit-bug-report "vm" "Send a bug report about VM." t)
