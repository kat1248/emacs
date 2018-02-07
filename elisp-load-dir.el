;;; @(#) elisp-load-dir.el -- Elisp Load Directory

;;{{{ Description

;; I really like the way RedHat Linux stores cron jobs in a directory
;; instead of a big crontab.  It makes cronjobs modular, and allows me
;; to add, modify, or delete one cronjob without impacting the others.

;; My .emacs file has grown more tangled than my cronjobs ever were.
;; For a while, I dealt with this by rewriting the file periodically
;; when it got too overgrown. A better solution is to break the .emacs
;; file into separate modules, stored in a common directory.  This
;; library makes that easy.

;;}}}

;;{{{ id

;; This file is *NOT* part of GNU emacs

;; Copyright (C) 1998 Len Budney <lbudney@pobox.com>
;; Author:       Len Budney
;; Maintainer:   Len Budney <lbudney@pobox.com>
;; Created:      1998
;; Thanks To:    Peter von der Ahe, Stefan Monnier, Saul Moskovitz

;; LCD Archive Entry:
;; elisp-load-dir|Len Budney|lbudney@pobox.com|
;; Elisp Load Directory|
;; 09-Aug-1996|1.0|~/misc/elisp-load-dir.el.Z|

;; COPYRIGHT NOTICE
;;
;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2 of the License, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with GNU Emacs.  If you did not, write to the Free Software Foundation,
;; Inc., 675 Mass Ave., Cambridge, MA 02139, USA.

;; INSTALLATION:
;;
;; Install elisp-load-dir.el somewhere on your load path.  Add the
;; line:
;;        (load-library elisp-load-dir)
;; to your .emacs file.  This makes available the following functions:
;;
;;    directory-elisp-files (DIR)    -- List elisp files in DIR
;;    elisp-load-dir (DIR)           -- Load each elisp library in DIR

;; USAGE:
;;
;; To modularize your .emacs file, create a directory (for example
;; "~/.emacsinit").  Put your initialization routines in that directory,
;; in separate elisp files.  You may byte-compile those files if you
;; wish.
;;
;; In your .emacs file, place the lines:
;;
;;         (load-library "elisp-load-dir")
;;         (elisp-load-dir "~/.emacsinit") ;; Rest of initialization here
;;
;; Now, for example, you can initialize variables and do other
;; miscellaneous setup in your .emacs file.  Large bodies of related
;; code, however, can be stored in a file in ~/.emacsinit.  Changing
;; your initialization is a matter of adding or removing files from
;; that directory.

;;}}}
(require 'cl)

;; Find the elisp files in a directory.
(defun directory-elisp-files (dir)
  "List files in a directory which are elisp files, based on their
extensions.  Returns a list of files, by full pathnames, without
extensions, ready to be supplied to the load command."
  (interactive "DDirectory name: ")
  (let
      ((dirfiles (mapcar
                 (lambda (string)
                   (string-match "^\\(.*\\)\\.elc?$" string)
                   (match-string 1 string))
                 (directory-files dir 'full "\\.elc?$"))))
    (remove-duplicates dirfiles :test 'equal)))

;; Load the elisp files in a directory
(defun elisp-load-dir (dir)
  "Load files in a directory which are elisp files.
Return value is always nil."
  (interactive "DDirectory name: ")
    (mapcar 'load (directory-elisp-files dir)))

;;; End of elisp-load-dir.el
