;;; Programming environment (C, C++, Perl, Elisp)
(load-library "cc-mode")

(setq-default cperl-indent-level 4)

;;(setq compilation-mode-font-lock-keywords
;;      (list
;;      (cons "^clearmake.*$" 'font-lock-comment-face)
;;       (cons "^Warning.*$" 'font-lock-string-face)
;;       (cons "^Error.*$" 'font-lock-function-name-face)
;;       ))

;(setq special-display-buffer-names
;      '(("*compilation*"
;	 (minibuffer . nil)
;	 (modeline . nil)
;	 (top-toolbar-visible-p . nil)
;	 (menubar-visible-p . nil)
;	 (height . 20) (width . 80) (top . 0) (left . 444))))

(defun my-compilation-mode-hook ()
  (truncate-mode 1))

;;  (setq font-lock-keywords compilation-mode-font-lock-keywords))
;  (if (eq (emacs-type) 'xemacs)
;      (turn-on-font-lock)))

(add-hook 'compilation-mode-hook 'my-compilation-mode-hook)

;;(setq compile-command "vx62; make")

;;(load "compile" t t)

;;(defconst wrs-c-style
;;  '("WRS Coding Conventions"
;;    (c-toggle-auto-state 	   . 1)
;;    (c-tab-always-indent           . t)
;;    (c-hanging-braces-alist        . ((block-open after before)
;;				      (brace-list-open)))
;;    (c-hanging-colons-alist        . ((member-init-intro before)
;;				      (inher-intro)
;;				      (case-label after)
;;				      (label after)
;;				      (access-key after)))
;;    (c-cleanup-list                . (scope-operator
;;				      empty-defun-braces
;;				      defun-close-semi))
;;    (c-offsets-alist               . ((statement-block-intro . 0)
;;				      (defun-open            . 2)
;;				      (arglist-close         . 0)
;;				      (arglist-intro         . 0)
;;				      (class-open            . 2)
;;				      (class-close           . 2)
;;				      (topmost-intro-cont    . 2)))
;;    (c-echo-semantic-information-p . nil)
;;    )
;;  "WRS Coding Conventions")

;;(defvar my-code-style
;;  '("My Coding Conventions"
;;    (c-offsets-alist               . ((substatement-open . 0)
;;				      (access-label . -1)
;;				      (block-open . -2)
;;				      (inline-open . 0)
;;				      (case-label . 2)))
;;    (c-basic-offset 		   . 2)
;;    (c-cleanup-list 		   . (brace-else-brace))
;;    (c-echo-semantic-information-p . nil)
;;    (c-hanging-braces-alist        . ((block-open . (after))))
;;    )
;;  "My Coding Conventions")

;;(defun wrs-c-mode-style-hook ()
;;  ;; set up for my perferred indentation style, but only do it once
;;  (let ((my-style "WRS Coding Conventions"))
;;    (or (assoc my-style c-style-alist)
;;	(setq c-style-alist (cons wrs-c-style c-style-alist)))
;;    (c-set-style my-style)))

;;(defun my-c-mode-style-hook ()
;;  ;; set up for my perferred indentation style, but only do it once
;;  (c-set-offset 'block-open '-)
;;  (c-set-offset 'substatement-open '0)
;;  (c-set-offset 'member-init-intro '-)
;;  (c-set-offset 'topmost-intro-cont '+)
;;  (c-set-offset 'case-label '+)
;;  (c-set-offset 'access-label '0)
;;  (setq tab-width 8)
;;  (setq indent-tabs-mode nil)
;;  (setq c-tab-always-indent  t)
;;  (setq c-basic-offset 2))

;;(add-hook 'c-mode-common-hook 'my-c-mode-style-hook)
;;(add-hook 'c-mode-common-hook 'wrs-c-mode-style-hook)

(defun ifdef-region ()
  "Surround region with `#ifdef KAT' and `#endif)'."
  (interactive)
  (kill-region (point) (mark))
  (insert "#ifdef KAT\n")
  (yank)
  (insert "#endif\n"))

(defun my-common-c-mode-hook ()
  (define-key c-mode-base-map "\C-m" 'reindent-then-newline-and-indent)
  (define-key c-mode-base-map [(meta left)] 'backward-list)
  (define-key c-mode-base-map [(meta right)] 'forward-list)
  ;;(define-key c-mode-base-map [(control meta ?;)] 'ifdef-region)
  (setq comment-column 50)
  (setq c-basic-offset 4)
  (setq indent-tabs-mode nil)		; Use spaces - not tabs
  ;;(modify-syntax-entry ?# "w")
  ;;(modify-syntax-entry ?_ "w")
  (truncate-mode 1)
;;  (subword-mode 1)
  ) 

(add-hook 'c-mode-common-hook 'my-common-c-mode-hook)

(defun my-c-mode-colors ()
  (interactive)
  (add-spec-to-specifier (face-property 'default 'background) 
                         "gray80" 
                         (current-buffer))
  (add-spec-to-specifier (face-property 'default 'foreground) 
                         "black" 
                         (current-buffer)))

;;(add-hook 'c-mode-common-hook 'my-c-mode-colors)

;;
;; acme packet style

(setq-default indent-tabs-mode nil)
(setq compile-command "make jobs=4")
(defun my-compile ()
  "Switch to compilation buffer and compile."
  (interactive)
  (get-buffer-create "*compilation*")
  (if (string-equal (buffer-name (current-buffer)) "*compilation*")
      (message "current buffer name = %s" (buffer-name (current-buffer)))
      (switch-to-buffer-other-window "*compilation*"))
  (call-interactively 'compile))

(defconst acme-c-style
  '((c-tab-always-indent        . t)
    (c-basic-offset             . 2)
    (c-indent-level             . 2)
    (c-comment-only-line-offset . 0)
    (c-hanging-braces-alist     . ((substatement-open after)
                                   (inline-open after)
                                   (statement-case-open after)
                                   (brace-list-open after)))
    (c-hanging-colons-alist     . ((member-init-intro before)
                                   (inher-intro)
                                   (case-label after)
                                   (label after)
                                   (access-label after)))
    (c-cleanup-list             . (scope-operator
                                   empty-defun-braces
                                   defun-close-semi))
    (c-offsets-alist            . ((arglist-close        . c-lineup-arglist)
                                   (substatement-open    . 0)
                                   (inline-open          . 0)
                                   (case-label           . 2)
                                   (statement-case-open  . 4)
                                   (statement-case-intro . 2)
                                   (block-open           . 0)
                                   (knr-argdecl-intro    . -)))
    (c-echo-syntactic-information-p . t))
  "ACME C Programming Style")

;; offset customizations not in my-c-style
(setq c-offsets-alist '((member-init-intro . ++)))

;; Customizations for all modes in CC Mode.
(defun acme-c-mode-common-hook ()
  ;; add my personal style and set it for the current buffer
  (c-add-style "ACME" acme-c-style t)
  ;; other customizations
  (setq tab-width 2
	;; use combinations of tabs and spaces when indenting
	indent-tabs-mode nil)
  ;; we like auto-newline and hungry-delete
  (c-toggle-auto-hungry-state -1))

(add-hook 'c-mode-common-hook 'acme-c-mode-common-hook)
(add-hook 'c++-mode-common-hook 'acme-c-mode-common-hook)

;;(require 'hideshow)
;;(defun SEH-hideshow-setup-hook ()
;;  "Enables hideshow and binds some commands"
;;  (hs-minor-mode 1)
;;  (define-key hs-minor-mode-map [(control c) (h)] 'hs-hide-block)
;;  (define-key hs-minor-mode-map [(control c) (s)] 'hs-show-block)
;;  (define-key hs-minor-mode-map [(control c) (H)] 'hs-hide-all)
;;  (define-key hs-minor-mode-map [(control c) (S)] 'hs-show-all)
;;  (define-key hs-minor-mode-map [(control c) (R)] 'hs-show-region)
;;  (define-key hs-minor-mode-map [f5] 'hs-hide-block)
;;  (define-key hs-minor-mode-map [f6] 'hs-show-block)
;;  (define-key hs-minor-mode-map [f7] 'hs-hide-all)
;;  (define-key hs-minor-mode-map [f8] 'hs-show-all))
 
;;(add-hook 'c-mode-common-hook 'SEH-hideshow-setup-hook t)
;;(add-hook 'c-mode-common-hook 'turn-on-setnu-mode)

;; Unisphere style guidlines
;(setq-default tab-width 4)		; set tab stops every four characters
;(defun RS-c-mode-common-hook ()
;  (c-set-style "stroustrup")		; use Stroustrup-style indentation
;  (setq c-basic-offset 4)		; used when calculating indentation
;  (c-set-offset 'case-label '+)		; indent case labels relative to switch
;  (setq tab-width 4)			; set tab stops every four characters
;  (setq fill-column 80)			; fill comments to column 80
;  (setq indent-tabs-mode nil)		; Use spaces - not tabs
;  )
;(add-hook 'c-mode-common-hook 'RS-c-mode-common-hook)

;;; lisp stuff

;;; Add hook for highlighting on the fly in emacs-lisp
(defun my-emacs-mode-hook ()
  (define-key emacs-lisp-mode-map "\C-m" 'reindent-then-newline-and-indent)
  (define-key emacs-lisp-mode-map "\M-\t" 'indent-region))

(load "cl-indent")
(setq lisp-indent-function (function common-lisp-indent-function))

(add-hook 'emacs-lisp-mode-hook 'my-emacs-mode-hook)

;; (add-hook 'emacs-lisp-mode-hook 'turn-on-setnu-mode)

;;; Auto byte compile elisp files
(defvar running-emacs-19 (string-match "^19" emacs-version))
(defmacro emacs-18 (&rest body)
  (` (if (not running-emacs-19)
	 (progn
	   (,@ body)))))
(defmacro emacs-19 (&rest body)
  (` (if running-emacs-19
	 (progn
	   (,@ body)))))

;;; Ask to compile .el and .scm files after saving them.
;;; In Emacs 18, this depends on Roland McGrath's after-save.el.

(setq after-save-hook
      '(run-mode-specific-after-save-buffer-hooks))

(defvar mode-specific-after-save-buffer-hooks nil
  "Alist (MAJOR-MODE . HOOK) of after-save-buffer hooks specific to major modes.")

(defun run-mode-specific-after-save-buffer-hooks ()
  "Run hooks in `mode-specific-after-save-buffer-hooks' that match the
current buffer's major mode.  To be put in `after-save-buffer-hooks'."
  (let ((hooks mode-specific-after-save-buffer-hooks))
    (while hooks
      (let ((hook (car hooks)))
	(if (eq (car hook) major-mode)
	    (funcall (cdr hook))))
      (setq hooks (cdr hooks)))))

(setq mode-specific-after-save-buffer-hooks
      '((emacs-lisp-mode . ask-to-byte-compile)))
;;	(scheme-mode . ask-to-scheme-compile)))

(defun ask-to-byte-compile ()
  "Ask the user whether to byte-compile the current buffer
if its name ends in `.el' and the `.elc' file also exists."
  (let ((name (buffer-file-name)))
    (and name (string-match "\\.el$" name)
	 (file-exists-p (concat name "c"))
	 (if (y-or-n-p (format "Byte-compile %s? " name))
	     (byte-compile-file name)
	   (message ""))
	 )))

;;(defun ask-to-scheme-compile ()
;;  "Ask the user whether to byte-compile the current buffer
;;if its name ends in `.scm' and the `.elc' file also exists."
;;  (let* ((name (buffer-file-name))
;;	 (root-name (and name
;;			 (string-match "\\(.*\\)\\.scm$" name)
;;			 (match-string 1 name))))
;;    (if root-name
;;	(cond ((file-exists-p (concat root-name ".com"))
;;	       (if (y-or-n-p (format "cf %s? " name))
;;		   (progn 
;;		     (xscheme-send-string (format "(cf \"%s\")" name))
;;		     (message "Producing %s.com...continue at will" root-name))
;;		 (message "")))
;;	      ((file-exists-p (concat root-name ".bin"))
;;	       (if (y-or-n-p (format "sf %s? " name))
;;		   (progn
;;		     (xscheme-send-string (format "(sf \"%s\")" name))
;;		     (message "Producing %s.bin...continue at will" root-name))
;;		 (message "")))))))

;;; View tags other window
(defun view-tag-other-window (tagname &optional next-p regexp-p)
  "Same as `find-tag-other-window' but doesn't move the point"
  (interactive (find-tag-interactive "View tag other window: "))
  (let ((window (get-buffer-window)))
    (find-tag-other-window tagname next-p regexp-p)
    (recenter 0)
    (select-window window)))

;(require 'go-mode)

