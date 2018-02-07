;;; Shell mode stuff
;(add-hook 'comint-output-filter-functions 'shell-strip-ctrl-m)
;(remove-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)
;(remove-hook 'comint-output-filter-functions 'shell-truncate-buffer-filter)

;;; code to keep the shell-mode buffer from growing indefinitely
(defun truncate-buffer (n)
 "Delete all but the last N lines of the current buffer."
 (save-excursion
   (goto-char (point-max))
   (forward-line (- n))
   (beginning-of-line)
   (delete-region (point-min) (point))))

(defun my-shell-newline ()
  (interactive)
  (goto-char (point-max))
  (comint-send-input))

(defun my-previous-input ()
  (interactive)
  (cond ((comint-after-pmark-p)
	 (goto-char (point-max))
	 (comint-previous-input 1))
	(t (pc-select-move-line-up 1))))

(defun my-next-input ()
  (interactive)
  (cond ((comint-after-pmark-p)
	 (goto-char (point-max))
	 (comint-next-input 1))
	(t (pc-select-move-line-down 1))))

(defun my-redo-last ()
  (interactive)
  (comint-next-prompt 1)
  (my-previous-input))

(defun my-save-command-output (file)
  (interactive "FSave output to file? ")
  (save-excursion
    (comint-previous-prompt 1)
    (beginning-of-line)
    (let ((beg (point)))
      (comint-next-prompt 1)
      (beginning-of-line)
      (write-region beg (point) file))))


(defun my-shell-mode-hook ()
  (setq comint-prompt-regexp "^[A-Z]:[^#$%>\n ]*[#$%>] ")
  (setq shell-mode-p t)
  (make-variable-buffer-local 'frame-title-format)
  (setq frame-title-format '(default-directory default-directory))
  (define-key shell-mode-map [up] 'my-previous-input)
  (define-key shell-mode-map [down] 'my-next-input)
  (define-key shell-mode-map [(control meta up)] 'my-redo-last)
  (define-key shell-mode-map [(meta up)] 'comint-previous-prompt)
  (define-key shell-mode-map [(meta down)] 'comint-next-prompt)
  (define-key shell-mode-map [(control a)] 'comint-bol)
  (define-key shell-mode-map [(control m)] 'my-shell-newline)
  (define-key global-map     [(control c) (w)] 'my-save-command-output))

(add-hook 'shell-mode-hook 'my-shell-mode-hook)

(defadvice comint-dynamic-complete (after remove-completions-window activate)
  (if (and (not ad-return-value)
	   (equal "*Completions*" (save-window-excursion 
				    (other-window 1)
				    (buffer-name))))
      (delete-window (get-buffer-window "*Completions*"))))

(defun my-shell-colors ()
  (interactive) 
  (add-spec-to-specifier (face-property 'default 'background) 
                         "black" 
                         (current-buffer))
  (add-spec-to-specifier (face-property 'default 'foreground) 
                         "green" 
                         (current-buffer))
  (install-shell-fonts)
  ;; (set-face-font 'shell-output "Courier New:Regular:12::Western")
  (set-face-foreground 'shell-prompt "yellow")
  (set-face-foreground 'shell-input "white"))

(add-hook 'shell-mode-hook 'my-shell-colors)

(defvar shell-max-buffer-size 10000
  "*Max size in lines of shell buffer")

(defun shell-truncate-buffer-filter (str)
  (truncate-buffer shell-max-buffer-size))

(defun shell-truncate-buffer ()
  (interactive)
  (truncate-buffer shell-max-buffer-size))

;(add-hook 'shell-mode-hook 
;	  '(lambda () (add-hook 'comint-output-filter-functions
;			'shell-truncate-buffer-filter)))

;(setq my-shell-font-lock-keywords
;      (append
;       shell-font-lock-keywords
;       (list
;	(cons "^\\[a-zA-Z0-9\\]*\\.\\(cc\\|h\\):\\[0-9\\]*:.*$"
;	      'font-lock-comment-face))))
;(setq shell-font-lock-keywords my-shell-font-lock-keywords)
