;;; .xemacs/init.el

;;; uncomment this line to disable loading of "default.el" at startup
;; (setq inhibit-default-init t)

(add-to-list 'load-path (expand-file-name "~/lib/emacs"))
(add-to-list 'load-path (expand-file-name "~/lib/emacs/icicles"))
(add-to-list 'load-path (expand-file-name "~/lib/emacs/go-mode"))

(setq inhibit-startup-message t)

(load-library "elisp-load-dir")
(elisp-load-dir "~/lib/emacs/init")

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(c-basic-offset 2)
 '(fringe-mode 0 nil (fringe))
 '(grep-find-command "find . -type f -not -name \"*.svn-base\" -print0 | xargs -0 -e grep -nH -I -e ")
 '(tool-bar-mode nil)
 '(transient-mark-mode (quote (only . t))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "gray90" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 90 :width normal :foundry "unknown" :family "DejaVu Sans Mono")))))
