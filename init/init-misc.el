;; miscellaneuos function definitions

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun scratch ()
  "Creates and/or selects a proper *scratch* buffer."
  (interactive)
  (get-buffer-create "*scratch*")
  (switch-to-buffer-other-window "*scratch*")
  (setq default-directory "~/")
  (funcall initial-major-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun open-line-above (p)
  "Insert a new line above the current one and indent accordingly."
  (interactive "p")
  (beginning-of-line)
  (open-line p)
  (indent-according-to-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun indent-region-or-line (arg)
  (interactive "p")
  (if (region-active-p)
      (indent-region (region-beginning) (region-end) nil)
      (indent-according-to-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar truncate-mode-hook nil
  "*If non-nil, a function or list of functions that are executed after
`truncate-mode' is turned on.")

(defun truncate-mode (&optional minor-mode-arg)
  "*Toggles truncate minor mode by toggling the value of `truncate-lines'.
With optional ARG, turns truncate mode on iff ARG \(prefix arg, if
called interactively\) is positive.  See `truncate-mode-hook' also."
  (interactive "P")
  (setq truncate-lines
	(if (null minor-mode-arg)
	    (not truncate-lines)
	    (> (prefix-numeric-value minor-mode-arg) 0)))
  (recenter nil)
  (if truncate-lines
      (run-hooks 'truncate-mode-hook)))

(or (memq 'truncate-lines minor-mode-alist)
    (setq minor-mode-alist
	  (cons (list 'truncate-lines " Trunc") minor-mode-alist)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; lock step scrolling
(defun kat-sync-scroll-up ()
  "Scrolls current and other window up by one line"
  (interactive)
  (scroll-down 3)
  (scroll-other-window-down 3))

(defun kat-sync-scroll-down ()
  "Scrolls current and other window down by one line"
  (interactive)
  (scroll-up 3)
  (scroll-other-window 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-deactivate-region ()
  (interactive)
  (zmacs-deactivate-region))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar last-popup-point nil
  "Last place we selected the popup menu")

(defun popup-edit-menu (e)
  "Pops up the default edit menu at the mouse position"
  (interactive "@e")
  (setq last-popup-point (mouse-position-as-motion-event))
  (popup-menu default-popup-menu))

(defadvice yank-clipboard-selection (around yank-clipboard-selection-at-mouse-point activate)
  "Set point to mouse point"
  (interactive "*")
  (save-excursion
    (mouse-set-point last-popup-point)
    ad-do-it))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun switch-to-vm ()
  (interactive)
  (if (get-buffer "inbox Summary")
      (switch-to-buffer "inbox Summary")
      (vm)))

(defun switch-to-vm-or-gnus ()
  "Switch to VM if not there, o.w. switch to GNUS"
  (interactive)
  (let ((buf (current-buffer))
	(vm-buf (get-buffer "inbox"))
	(vm-sum-buf (get-buffer "inbox Summary"))
	(gnus-buf (get-buffer "*Group*")))
    (cond ((or (eq buf vm-sum-buf)	;if we are looking at mail, switch to news
	       (eq buf vm-buf))
	   (if gnus-buf
	       (switch-to-buffer "*Group*")
	       (gnus)))
	  (t				;otherwise, read our mail
	   (if vm-buf
	       (switch-to-buffer "inbox Summary")
	       (cond ((y-or-n-p "Start VM? ")
		      (vm))
		     (t (message "Ooops!"))))))
    (delete-other-windows)		;remove distractions
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'ffap)

(defun find-file-at-mouse ()
  (interactive)
  (save-excursion
    (mouse-set-point (mouse-position-as-motion-event))
    (let ((file (ffap-file-at-point)))
      (if file
	  (find-file-other-window file)
	  (message "No filename found")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-other-window ()
  (interactive)
  (other-window 1 'visible))

(defun my-other-window-back ()
  (interactive)
  (other-window -1 'visible))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-home ()
  "If current column is greater than 0, goes to column 0.
Otherwise, if not at top of current window, goes to top of window.
Otherwise, goes to beginning of buffer."
  (interactive)
  (if (> (current-column) 0)	
      (if (eq last-command 'my-home)
	  (beginning-of-line)
	  (beginning-of-line-text))
      (if (not (eq (point) (window-start)))
	  (move-to-window-line 0)
	  (goto-char (point-min)))))
	
(defun my-end ()
  "If not at end of current line, goes to end of line.
Otherwise, if not at bottom of current window, goes to bottom of window.
Otherwise, goes to end of buffer."
  (interactive)
  (let ((oldpoint (point)))
    (if (not (eolp))
	(end-of-line)
	(move-to-window-line -1)
	(end-of-line)
	(if (eq oldpoint (point))
	    (goto-char (point-max))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(defun my-print () 
;  "Print the current buffer"
;  (interactive)
;  (save-excursion
;    ;; gnus special case
;    (if (or (string= "*Subject*" (buffer-name))
;	    (string= "*Summary*" (buffer-name)))
;	(set-buffer "*Article*"))
;    ;; rmail-summary-mode special case 
;    (if (eq 'rmail-summary-mode major-mode)
;	(set-buffer rmail-buffer))
;    (if (eq 'vm-summary-mode major-mode)
;	(other-window 1)) ;; hack
;    (enscript-region (point-min) (point-max))
;    (if (eq major-mode 'rmail-mode)
;	(rmail-add-label "printed"))
;    (if (eq 'vm-summary-mode major-mode)
;	(other-window 1)) ;; hack
;    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun save-kbd-macro (name arg)
    "Save current kbd macro in .emacs, using NAME. With ARG, save .emacs"
    (interactive "SName for last keyboard macro: \nP")
    (name-last-kbd-macro name)
    (set-buffer (find-file-noselect "~/.emacs"))
    (goto-char (point-max))
    (insert-kbd-macro name)
    ; optional:
    (and arg (save-buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; delete frame or exit emacs
(defun my-clean-exit-from-emacs ()
  (interactive)
  (if ;;(or (not window-system)
          (yes-or-no-p "Exit Emacs? ");;)
      (save-buffers-kill-emacs)))

(defun my-exit-from-emacs ()
   (interactive)
   (if (cdr (frame-list))
      (if (yes-or-no-p "Delete this frame? ")
           (delete-frame))
      (my-clean-exit-from-emacs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun revert-all-buffers ()
  "Revert all non-modified buffers."
  (interactive)
  (save-excursion
    (mapcar
     '(lambda (b)
       (set-buffer b)
       (if (and (buffer-file-name (current-buffer))
		(not (buffer-modified-p (current-buffer))))
	   (revert-buffer t t)))
     (buffer-list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-all-files (regexp)
  "Find multiple files in one command."
  (interactive "sFind files matching regexp (default all): ")
  (when (string= "" regexp) (setq regexp ""))
  (let ((dir (file-name-directory regexp))
        (nodir (file-name-nondirectory regexp)))
    (when dir (cd dir))
    (when (string= "" nodir) (setq nodir "."))
    (let ((files (directory-files "." t nodir nil t))
          (errors 0))
      (while (not (null files))
        (let ((filename (car files)))
          (if (file-readable-p filename)
              (find-file-noselect filename)
            (incf errors))
          (setq files (cdr files))))
      (when (> errors 0)
        (message (format "%d files were unreadable." errors))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun zap-up-to-char (arg char)
  "Kill up to, but not including ARGth occurrence of CHAR.
   Case is ignored if `case-fold-search' is non-nil in the current buffer.
   Goes backward if ARG is negative; error if CHAR not found.
   Ignores CHAR at point."
  (interactive "p\ncZap up to char: ")
  (let ((direction (if (>= arg 0) 1 -1)))
    (kill-region (point)
                 (progn
                   (forward-char direction)
                   (unwind-protect
                          (search-forward (char-to-string char) nil nil arg)
                     (backward-char direction))
                   (point)))))
