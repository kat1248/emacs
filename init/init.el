;; load and init various modes...
;;(require 'ssh)
;;(setq ssh-program "c:/Program Files/NetworkSimplicity/ssh/ssh.exe"
;;      ssh-explicit-args '("-l kat")
;;      ssh-remote-user "kat")

;; pc type selection
;;(require 'pc-select)
;;(pc-select-mode 1)

;; my clearcase stuff
;;(load-library "clearcase")

(require 'package)
;; Any add to list for package-archives (to add marmalade or melpa) goes here, e.g.:
(add-to-list 'package-archives 
    '("marmalade" .
      "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)
(package-initialize)

(load-library "goto-last-change")

;; font-lock stuff
(require 'font-lock)
;(set-face-foreground 'modeline "black")
;(set-face-background 'modeline "gray60")
(defun load-my-faces ()
  (interactive)
  (set-face-background 'default "gray90")
  (set-face-foreground 'font-lock-string-face "darkgreen")
  (set-face-foreground 'font-lock-comment-face "red")
  (set-face-foreground 'font-lock-keyword-face "purple")
  ;;(set-face-foreground 'font-lock-reference-face "darkblue")
  (set-face-foreground 'font-lock-variable-name-face "seagreen")
  (set-face-foreground 'font-lock-function-name-face "gray20")
  (set-face-foreground 'font-lock-type-face "darkblue"))

(add-hook 'after-init-hook 'load-my-faces)

;;(load-library "monokai-dark-soda-theme")
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 'buffers)

;; max out my menus
;;(load-library "big-menubar")

;; function menu stuff
;;(require 'func-menu)
;;(setq fume-max-items 40)
;;(add-hook 'find-file-hooks 'fume-add-menubar-entry)
;;(add-hook 'c-mode-common-hook
;;	  '(lambda ()
;;	    (setq fume-display-in-modeline-p t)))

;; add a 'recent files' menu
;(load "recent-files")
;(setq recent-files-dont-include
;      '("\\.newsrc" "~$" "\\.KILL$" "pop.crash$"
;	"\\.el$" "\\.elc$" "\\.vm" "\\.emacs" "\\.bbdb" "inbox$")
;      recent-files-add-menu-before "Help"
;      recent-files-sort-function 'recent-files-sort-alphabetically
;      recent-files-permanent-submenu t
;      recent-files-non-permanent-submenu nil
;      recent-files-commands-submenu t)
;(recent-files-initialize)

;; resize the minibuffer if necessary
;;(require 'rsz-minibuf)
;;(resize-minibuffer-mode)

;; colorize info mode
;;(add-hook 'Info-mode-hook
;;	  '(lambda ()
;;	    ;; Using color so don't want italics
;;	    (set-face-font 'info-node (face-font (get-face 'default)))
;;	    (set-face-font 'info-xref (face-font (get-face 'default)))
;;	    (set-face-foreground 'info-node "blue")
;;	    (set-face-foreground 'info-xref "red")))
      

;; advanced switch buffer
(require 'sje-switch-buffer)
(sje-switch-buffer-default-keybindings)
(setq sje-switch-buffer-confirm 'if-multiple)

;; delete all whitespace
;;(require 'greedy-delete)
;;(add-hook 'text-mode-hook 'gd-add-to-mode)
;;(add-hook 'c-mode-hook 'gd-add-to-mode)

;; fill adapt mode
;(require 'filladapt)
;(require 'fa-extras)
;(setq-default filladapt-mode t)
;(add-hook 'text-mode-hook 'turn-on-filladapt-mode)
;(add-hook 'c-mode-hook 'turn-off-filladapt-mode)

;; hscroll
;(load-library "hscroll")
;(setq-default hscroll-mode t)
;(hscroll-mode)
;(setq hscroll-margin 1)

;; icomplete
(require 'icomplete)
(icomplete-mode)

;;(load-library "light")

;;(load-library "ispell-mode")
;;(setq ispell-query-replace-choices t)

;; save place
(require 'saveplace)

;;(require 'w3-auto "w3-auto")
   
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

;;; various setq's to get things the way I like it

(add-hook 'text-mode-hook 'turn-on-auto-fill)

;(set-specifier top-toolbar-height 0) 
;(set-specifier horizontal-scrollbar-visible-p nil)
;(set-specifier menubar-visible-p t)

(setq auto-raise-frame nil
      completion-ignore-case t
      read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t
;      default-buffer-file-coding-system 'no-conversion-dos
      default-major-mode 'text-mode
      diary-display-hook 'fancy-diary-display
      float-output-format "%.5g"
      highlight-nonselected-windows nil
      kill-whole-line t
      mark-even-if-inactive t
      menubar-show-keybindings nil
      minibuffer-max-depth nil
      mswindows-alt-by-itself-activates-menu nil
      next-line-add-newlines nil
      require-final-newline t
      same-window-buffer-names (append '("*Buffer List*") same-window-buffer-names)
      scroll-step 1
      search-highlight t
      split-window-keep-point t
      truncate-partial-width-windows t
      visible-bell nil)

;(require 'dip-mode)
;(defvar spassky-vars
;  `((dip-country-abbrev . "e")
;    (dip-country-full . "England")
;    (dip-pass . "1248")
;    (dip-game . "spassky")
;    (dip-goodbye . "Regards,")
;    (dip-hello . "Greetings!")
;    (dip-variant . "")
;    (dip-judge . ,dip-judge-usef))
;  "Spassky on USEF")

;(defvar elector-vars
;  `((dip-country-abbrev . "f")
;    (dip-country-full . "France")
;    (dip-pass . "1248")
;    (dip-game . "elector")
;    (dip-goodbye . "Regards,")
;    (dip-hello . "Greetings!")
;    (dip-variant . "gunboat")
;    (dip-judge . ,dip-judge-usin))
;  "Elector on USIN")

;(setq dip-games
;      '(
;	("spassky"  spassky-vars)
;	;("elector"  elector-vars)
;	))

(setq-default case-fold-search t
	      ediff-ignore-similar-regions t
	      save-place t)

(put 'eval-expression 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Disable nuisance enabled commands:
(put 'set-goal-column 'disabled t)
(put 'set-fill-column 'disabled t)
(put 'rmail 'disabled t)

;;; setup auto-mode lists
(autoload 'lsl-mode "lsl-mode" "Load LSL mode." t)

(setq auto-mode-alist (cons (cons "\\.c\\+\\+$" 'c++-mode) auto-mode-alist))
(setq auto-mode-alist (cons (cons "\\.cc$" 'c++-mode) auto-mode-alist))
(setq auto-mode-alist (cons (cons "\\.c$" 'c++-mode) auto-mode-alist))
(setq auto-mode-alist (cons (cons "\\.cxx$" 'c++-mode) auto-mode-alist))
(setq auto-mode-alist (cons (cons "\\.csn$" 'c++-mode) auto-mode-alist))
(setq auto-mode-alist (cons (cons "\\.h$" 'c++-mode) auto-mode-alist))
(setq auto-mode-alist (cons (cons "\\.l$" 'c-mode) auto-mode-alist))
(setq auto-mode-alist (cons (cons "\\.lsl$" 'lsl-mode) auto-mode-alist))
(setq auto-mode-alist (cons (cons "\\.x$" 'c-mode) auto-mode-alist))

(setq completion-ignored-extensions
      '(".o" ".elc" "~" ".bin" ".lbin" ".fasl" ".sym" ".smp"
        ".dvi" ".toc" ".log" ".aux" ".v"
        ".lof" ".blg" ".bbl" ".glo" ".idx" ".lot"))

;;(cond ((y-or-n-p "Start gnuserv? ")
;;       (load-library "gnuserv")
;;       (setq gnuserv-frame 'gnuserv-visible-frame-function)
;;       ;(setq gnuserv-frame (selected-frame))
;;       (gnuserv-start))
;;      (t (message "Not starting gnuserv")))

;;(cond ((y-or-n-p "Restore emacs state? ")
;;       (load "desktop")
;;       (desktop-load-default)
;;       (desktop-read)
;;       (add-hook 'kill-emacs-hook
;;		 '(lambda ()
;;		   (desktop-truncate search-ring 3)
;;		   (desktop-truncate regexp-search-ring 3))))
;;      (t (message "not using desktop")))      

;(setq initial-frame-plist '(minibuffer nil))
;(setq default-frame-plist '(minibuffer nil))
;(setq default-minibuffer-frame
;      (make-frame
;       '(minibuffer only
;                    width 86
;                    height 1
;                    menubar-visible-p nil
;                    default-toolbar-visible-p nil
;                    name "minibuffer"
;                    top -2
;                    left -2
;                    has-modeline-p nil)))
;(frame-notice-user-settings)

(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

(autoload 'lojban-parse-region "lojban" nil t)
(autoload 'lojban-mode "lojban-mode" nil t)
