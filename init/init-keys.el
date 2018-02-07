(define-key global-map [(control o)]	 'open-line-above)
(define-key global-map [(control h)]	 [delete])
(define-key global-map [(meta h)]	 help-map)
(define-key global-map [(control /)]	 'advertised-undo)
  
(define-key global-map "\C-cg"	 	 'fume-prompt-function-goto)
(define-key global-map "\r"		 'newline-and-indent)

(define-key global-map [(control meta ?`)] 'shell)
(define-key global-map [(meta ?`)] 	 'scratch)

(define-key global-map [(control meta button1)] 'find-file-at-mouse)

(define-key global-map [(control meta y)] 'clipboard-yank)
(define-key global-map [(meta w)] 'clipboard-kill-ring-save)

(define-key global-map [(button3)]	 'popup-edit-menu)
(define-key global-map [(control button3)] 'popup-mode-menu)
(define-key global-map [(meta button3)]  'mouse-function-menu)
(define-key global-map [(shift button3)] 'popup-buffer-menu)

(define-key global-map [(control next)]  'kat-sync-scroll-down)
(define-key global-map [(control prior)] 'kat-sync-scroll-up)

(define-key global-map [(meta up)]	 'scroll-up)
(define-key global-map [(meta down)]	 'scroll-down)
(define-key global-map [(meta left)]	 'scroll-left)
(define-key global-map [(meta right)]    'scroll-right)
(define-key global-map [(control up)]    'scroll-up-one-line)
(define-key global-map [(control down)]  'scroll-down-one-line)
(define-key global-map [(control left)]  'scroll-left-one-column)
(define-key global-map [(control right)] 'scroll-right-one-column)

(define-key global-map [(control tab)]	 'my-other-window)
(define-key global-map [(meta control tab)] 'my-other-window-back)

(define-key global-map [(meta /)]	 'my-deactivate-region)
(define-key global-map [(meta ?z)]	 'zap-up-to-char)

(define-key global-map [f10] 		 'bbdb)
(define-key global-map [f11] 		 'bbdb-create)

(define-key ctl-x-map  "\r"  		 'switch-to-vm-or-gnus)
(define-key ctl-x-map  "m" 		 'vm-mail)
(define-key ctl-x-map  "t" 		 'truncate-mode)
(define-key ctl-x-map  "c"       	 'my-compile) 
(define-key ctl-x-map  "w"       	 'copy-to-register) 
(define-key ctl-x-map  "y"       	 'insert-register) 
(define-key ctl-x-map  "\C-k"    	 'bury-buffer)
(define-key ctl-x-map  [(control ?c)] 	 'my-exit-from-emacs)

(define-key esc-map    "g"       	 'goto-line)
(define-key esc-map    "+"       	 'enlarge-window)
(define-key esc-map    "="       	 'shrink-window)

(defun load-my-key-defs ()
  (interactive)
  (define-key pc-select-map [home]	 'my-home)
  (define-key pc-select-map [end] 	 'my-end))

;;(add-hook 'after-init-hook 'load-my-key-defs)
