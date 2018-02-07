;;;(load-library "my-gnus");
;;;(setq gnus-check-new-newsgroups 'ask-server
;;;      gnus-read-active-file nil
;;;      gnus-nov-is-evil nil)

(setq gnus-local-distributions '("world" "local"))
(setq gnus-local-domain "sprynet.com")
(setq gnus-local-organization "SpryNet")
(setq gnus-nntp-server "enews.newsguy.com")
(setq gnus-use-generic-from t)
(setq gnus-your-domain "sprynet.com")
(setq gnus-your-organization "None That I Know Of")
(setq user-full-name "Ken Taylor")
(setq user-mail-address "ktaylor@pobox.com")

(setq gnus-group-line-format "%M%S%p%P%5y: %(%-25G%) %l\n")

;; dont keep a record of killed groups
(setq gnus-save-killed-list nil)
;; trust the server to tell us about new groups  
(setq gnus-check-new-newsgroups 'ask-server)
;; only read as much of the active file as we need
(setq gnus-read-active-file t)

(setq
 gnus-home-directory        (expand-file-name "~/.gnus")
 message-directory          (concat gnus-home-directory "/Mail")
 nndraft-directory          (concat gnus-home-directory "/drafts")
 gnus-startup-file          (concat gnus-home-directory "/newsrc"))

(setq gnus-auto-select-first nil
      gnus-auto-select-next 'quietly
      gnus-default-article-saver 'gnus-summary-save-in-file
      gnus-interactive-catchup nil
      gnus-large-newsgroup 200
      gnus-nov-is-evil nil
      gnus-save-score t
      gnus-show-threads t
      gnus-subscribe-newsgroup-method 'gnus-subscribe-alphabetically
      ;; Show tree structure by indenting the subject instead of the name.
      gnus-summary-line-format "%U%R%z %(%[%4L: %-20,20n%]%)%I %s\n"
      ;;  gnus-summary-line-format "%U%R%z %(%[%4L: %-20,20n%]%)%I %s\n")
      ;; Standard string indicating the the subject is unchanged.
      gnus-summary-same-subject "-\"-"
      gnus-thread-hide-subtree t
      gnus-thread-sort-functions '(gnus-thread-sort-by-subject)
;      gnus-uncacheable-groups "^alt.binaries"
      gnus-use-adaptive-scoring t
      gnus-use-cache t)

(setq nntp-warn-about-losing-connection nil)

;;(require 'highlight-headers)
;;(setq vm-highlighted-header-regexp "Subject:\\|From:"
;;      highlight-headers-hack-x-face-p t)
;;(copy-face 'default 'message-highlighted-header-contents)
;;(make-face-bold 'message-highlighted-header-contents)
;;(set-face-foreground 'message-headers "darkslateblue")
;;(set-face-foreground 'message-header-contents "firebrick")
;;(set-face-foreground 'message-cited-text "darkgreen")

(defun nntp-send-authinfo-quiet ()
  "Send the AUTHINFO to the nntp server.
This function is supposed to be called from `nntp-server-opened-hook'.
It will not prompt for anything."
  (nntp-send-command
   "^.*\r?\n" "AUTHINFO USER" "kat1248")
  (nntp-send-command
   "^.*\r?\n" "AUTHINFO PASS" "mnpibb"))

(add-hook 'nntp-server-opened-hook 'nntp-send-authinfo-quiet)

;(add-hook 'news-inews-hook 'ispell-message)
;(add-hook 'message-send-hook  'ispell-message)

;; Set some easier keybindings to make browsing more intuitive

(defun my-gnus-summary-show-thread ()
  "Show thread without changing cursor positon."
  (interactive)
  (gnus-summary-show-thread)
  (beginning-of-line)
  (forward-char 1))

;(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

(add-hook 'gnus-startup-hook
	  '(lambda ()
	    (define-key gnus-summary-mode-map [(right)] 'my-gnus-summary-show-thread)
	    (define-key gnus-summary-mode-map [(left)]  'gnus-summary-hide-thread)
	    (define-key gnus-summary-mode-map [(meta right)] 'gnus-summary-show-all-threads)
	    (define-key gnus-summary-mode-map [(meta left)]  'gnus-summary-hide-all-threads)
	    (define-key gnus-summary-mode-map [(control c) ?c]  'gnus-summary-catchup-to-here)
	    ))
