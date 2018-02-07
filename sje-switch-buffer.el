;;; sje-switch-buffer.el -- Switch between buffers using substrings

;;; Stephen Eglen, stephene@cogs.susx.ac.uk

;;; Overview

;;; This code provides a simple extension to the buffer switching
;;; functions so that you can specify the buffer using a substring as
;;; well as still using the full name of the buffer.

;;; Only tested on emacs 19.27


;;; LCD Archive Entry:
;;; sje-switch-buffer|Stephen Eglen|stephene@cogs.susx.ac.uk|
;;; Switch between buffers using substrings|
;;; 22-Apr-1996|$Revision: 1.2 $|path|

;;; COPYRIGHT NOTICE
;;;
;;; This program is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by the Free
;;; Software Foundation; either version 2 of the License, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;;; for more details.
;;;
;;; You should have received a copy of the GNU General Public License along
;;; with GNU Emacs.  If you did not, write to the Free Software Foundation,
;;; Inc., 675 Mass Ave., Cambridge, MA 02139, USA.


;;; Installation
;;;
;;; (require 'sje-switch-buffer)
;;; to load the functions.
;;;
;;; To override the default functions bound to C-x b, C-x 4 b, and
;;; C-x 5 b, call the following function
;;; (sje-switch-buffer-default-keybindings)

;;; How it works.

;;; When switching buffers, you can still use completion to select a
;;; buffer, or type in its full name.  However, you can also type a
;;; substring to specify a buffer.  So, if you have the following
;;; buffers (in order):

;;; file.c		(most recent)
;;; makefile
;;; file.l
;;; file.h
;;; file.data

;;; Typing "data" will take you to the most recent buffer containing
;;; the string "data", whereas "ake" would take you to the makefile.
;;; Using a substring rather than using the full name can be quicker
;;; when you can think of a useful substring (eg, type .c to go back
;;; to the most recent c file, shell, to take you back to the most
;;; *shell* buffer.)

;;; Also, with the confirm option, you can step through the buffer
;;; names matching your substring until you find the buffer that you
;;; are looking for. So, if you have three C files open, typing .c
;;; will give you the option of going to the newest C buffer.  If you
;;; dont want the newest C file, you can then go to the second newest
;;; buffer, and so on.

;;; Creating new buffers.
;;;
;;; If no buffer matches the substring, the default behaviour is not
;;; to create a new buffer called substring, unlike the normal
;;; switch-to-buffer function.  If C-u (with default arg of 4) is
;;; given, if the buffer name typed in does not exactly match an
;;; existing buffer, a new buffer is created.  To allow substring
;;; matching of buffers when you want to create new buffers also, set
;;; sje-switch-buffer-create-substring to a non nil value.  For
;;; example, by default if you want to create a new buffer "ail", you
;;; could type "ail", and if the buffer does not exist, it will be
;;; created.  However, if sje-switch-buffer-create-substring is set
;;; to a non nil value, the new buffer "ail" will only be created if
;;; "ail" is not a substring of any buffer name. So in this case, if
;;; there is a buffer called RMAIL, we will switch to this buffer,
;;; rather than create the new buffer "ail".

;;; Options
;;; -------
;;; The buffer switching has a couple of options coded as variables.

;;; Substring confirmation

;;; If confirm is nil, we switch to the newest buffer matching the
;;; substring.  Otherwise, the user will be prompted if the buffer
;;; matching the substring is the required buffer.  If the user
;;; answers negative, the next function continues to look for the next
;;; buffer matching the substring. 

;;; A tweak to the confirmation is that if sje-switch-buffer-confirm
;;; is set to 'if-multiple, then it will only ask for confirmation if
;;; there is more than one match.

;;; No confirming means that the user doesnt need an extra key press
;;; to confirm the switch to the new buffer.  However, with the
;;; confirm option, if the most recent buffer matching the substring
;;; is not the one you want, you get the chance to ignore the buffer
;;; in continue searching through the older buffers. 

;;; Substring searching - exact or regexp 

;;; By default, the substring search is an exact match, rather than a
;;; regexp match, but this can be changed by setting the variable
;;; sje-switch-buffer-regexp to a non nil value.  Regexp searching is
;;; more powerful, but then certain characters (most commonly . which
;;; appears quite often in filenames) need to be prefixed by a slash,
;;; eg if you are looking for the most recent .el file you would need
;;; to type \.el rather than just .el (because this may get you the
;;; shell buffer for example).

;;; Ignoring certain buffer names

;;; The regexp sje-switch-buffer-ignore can be set to ignore certain
;;; buffer names when it is looking through the substring match.  This
;;; normally is set to ignore file names beginning with a space. Eg to
;;; ignore substring searching in C source files as well you could do:
;;; (setq sje-switch-buffer-ignore  '("^ " "\\.c"))




;;; Prefix args:
;;; The following prefix args can be used to modify the effect of
;;; buffer switching.

;;; C-u 2 - toggle value of sje-switch-buffer-regexp before doing search
;;; C-u 3 - toggle value of sje-switch-buffer-confirm before doing search
;;; C-u 4 - create buffer if no buffer exists with the exact name "substring"

;;; When using these, C-u takes a default value of 4, so the 4 does not
;;; need to be specified.

;;; Thanks to many people for comments.  Copy should also be
;;; available on: http://www.cogs.susx.ac.uk/users/stephene/emacs/

;;; History


;;; Mon Apr 22 1996

;;; sje-switch-buffer-ignore regexp added, so that it ignores certain
;;; buffer names, eg those beginning with space.
;;; Added 'if-multiple option for the confirmation variable, so that
;;; if only one buffer matches substring, we do not ask for confirmation.
;;; Delayed evaluation of buffer-names until necessary (Thanks to
;;; Wayne Mesard <wmesard@esd.sgi.com> for mods).

;;; Mon Apr 15 1996 

;;; Added provide line and also taken keybindings out into a separate
;;; defun.  Thanks to Christian Moen <christim@ifi.uio.no>, and Jari
;;; Aalto <jaalto@tre.tele.nokia.fi> for suggesting this.

;;; Code


;;;  Variables storing defaults
(defvar sje-switch-buffer-ignore
  '("^ ")
  "*List of regexps matching buffer names to ignore.
For example, traditional behavior is not to list buffers whose names begin
with a space, for which the regexp is \"^ \".")

(defvar sje-switch-buffer-regexp nil
"*Control whether the sje-switch-buffer does an exact or regexp search.
Non nil values mean do regexp searching.")

(defvar sje-switch-buffer-confirm nil
"*If nil, we dont ask for confirmation for switching to the buffer.
If 'if-multiple, we only ask for confirmation if multiple buffers
matches substring.  Otherwise, always ask for confirmation.")



(defvar sje-switch-buffer-create-substring nil
"*Non nil value means that if we want to create new buffers, we will
still search first for buffers matching the substring before creating
a new buffer.")

;;; internal debug variable - set to t for debug in *op* buffer
(setq sje-debug nil)

;;; Defuns
(defun sje-switch-buffer ( &optional create split newframe)

  "Find next buffer matching a substring somewhere within the buffer
name. If prefix arg given and no buffer matches substring, create new
buffer.  If split is set, current window is split in two. If newframe
is set, a new frame is created to display the buffer in.  (Only one of
split or newframe can be specified.)  "

  (interactive "p")
  (let  
      ( ;(buffernames (mapcar (function buffer-name) (buffer-list)))
       ;(buffernames (cdr (buffer-list)))
       (buffernames (buffer-list))
       (nextbuffer nil)
       (buffer nil)
       (prompt nil)
       (defaultbuff (buffer-name (other-buffer (current-buffer))))
       (found nil)
       (switchcmd nil)
       (possible-buffer '() )
       )

    ;;; See if we want to toggle the value of the variables.

    (if (= create 2)
	;; then
	(setq sje-switch-buffer-regexp (not sje-switch-buffer-regexp)) )
    
    (if (= create 3)
	;; then
	(setq sje-switch-buffer-confirm 
	      (next-in-list sje-switch-buffer-confirm '(nil if-multiple t) )) )


    ;;; Decide on how we want to view the new buffer
    (if split
	(setq switchcmd 'pop-to-buffer)
      ;; else
      (if newframe
	  (setq switchcmd 'sje-pop-to-buffer-newframe)
	;; else
	;; normal switch
	(setq switchcmd 'switch-to-buffer)))
    (setq prompt (sje-switch-buffer-prompt))

    (setq buffer
	  (read-buffer
	   (format prompt)
	   defaultbuff))

    ;; see if the buffer is a real buffer first.
    (if (get-buffer buffer)
	;; then buffer is a real buffer so lets switch to it.
	(eval (list switchcmd buffer))
      ;; else 
      ;; buffer didnt exist, so lets do a substring search.
      (progn

	(if (and (not sje-switch-buffer-create-substring)
		 (= create 4))
	    ;; lets just create the new buffer.
	    (eval (list switchcmd buffer))
	  ;; else	
	  (progn
	    ;; Exact or regexp search?
	    (if (not sje-switch-buffer-regexp)
		(setq buffer (regexp-quote buffer)))
	    
	    (setq found nil)
	    (while ( and (not found) buffernames )

	      ;; iterate down the list of buffernames.
	      (setq nextbuffer (buffer-name (car buffernames)) )
	      (setq buffernames (cdr buffernames))	      
	      ;; look for a match.
	      (if sje-debug 
		  (print (format "looking at %s" nextbuffer) 
			 (get-buffer-create "op")))
	      (if (and (not (sje-ignore-buffername-p nextbuffer))
			    (string-match buffer nextbuffer))
		  ;; we have found a possible match
		  (progn
		    (if sje-debug
			(print "found a poss match" (get-buffer-create "op")))
		    (cond  ( (eq sje-switch-buffer-confirm 'if-multiple)
			     (setq possible-buffer 
				   (cons nextbuffer possible-buffer)))
			   (sje-switch-buffer-confirm
			    ;; then need a confirm
			    (if (y-or-n-p (format "switch to %s? " nextbuffer))
				(setq found nextbuffer)) )
			   (t
			    ;; else no need for confirmation
			    (setq found nextbuffer))
			   ); end cond
		    )
		    ) ;end if string-match
		) ; end while

	    ;; end of searching - lets see if we found a buffer.

	    ;; first of all, check to see if we used if-multiple
	    (if (eq sje-switch-buffer-confirm 'if-multiple)
		(if (eq 1 (length possible-buffer))
		    ;; only 1 choice to make from
		    ;; so we just pick that one.
		    (setq found (car possible-buffer))
		  ;; else we shall iterate down the list
		  ;; and ask the user...
		  ;; reverse destructively - guess it is faster?
		  (setq possible-buffer (nreverse possible-buffer))
		  (while (and (not found) possible-buffer)
		    (setq nextbuffer (car possible-buffer))
		    (if (y-or-n-p (format "switch to %s? " nextbuffer))
			(setq found nextbuffer)
		      ;; else go onto next.
		      (setq possible-buffer (cdr possible-buffer))
		      ) 
		    ) ; next while
		  )
	      ) ; end 'if-multiple
	    
	    ;; Now do the switch to the relevant buffer.  If a buffer
	    ;; was found, the varaible found stores the name of the
	    ;; buffer to switch to.
	    (if found
		(eval (list switchcmd found))
		;; else
		(if (and sje-switch-buffer-create-substring
			 (= create 4))
		    ;; lets just create the new buffer.
		    (eval (list switchcmd buffer))
		  ;; else
		  (message "no buffer matching %s selected" buffer)
		  )
		) 
	    ) ;end progn exact or regexp search
	  ) ))
    ) ; end let
) 

; adapted from files.el
(defun sje-switch-to-buffer-other-window (create)
  "Select buffer BUFFER in another window."
  (interactive "p")
  (sje-switch-buffer create 'split nil))



; adapted from files.el
(defun sje-switch-to-buffer-other-frame (create)
  "Select buffer BUFFER in another frame."
  (interactive "p")
  (let ((pop-up-frames t))
    (sje-switch-buffer create nil 'newframe)
    ))


;; this is copied from switch-to-buffer-other-frame from files.el
;; and is an aux. function.
(defun sje-pop-to-buffer-newframe (buffer)
  "Open up buffer in new frame"
  (let ((pop-up-frames t))
    (pop-to-buffer buffer t)
    (raise-frame (window-frame (selected-window)))))


;;; decide which kind of prompt you would like.
(defun sje-switch-buffer-prompt2 ()
  (format "switch buffer %s %s "
	  (if sje-switch-buffer-regexp "regexp" "exact")
	  (if sje-switch-buffer-confirm "confirm" "no confirm")))

(defun sje-switch-buffer-prompt ()
  (format "switch buffer %s %s "
	  (if sje-switch-buffer-regexp "r" "e")
	  (cond ( (eq sje-switch-buffer-confirm 'if-multiple) "i")
		(  sje-switch-buffer-confirm "c")
		(  t "n"))
	  ))


(defun sje-switch-buffer-default-keybindings ()
  (interactive)
  "Override the definitions of C-x b, C-x 4 b and C-x 5 b for buffer switching"
  (global-set-key "b" (quote sje-switch-buffer))
  (global-set-key "4b" (quote sje-switch-to-buffer-other-window))
  (global-set-key "5b" (quote sje-switch-to-buffer-other-frame))
)



;;; Aux functions.
;; taken from listbuf-ignore-buffername-p in listbuf.el 
;; by friedman@prep.ai.mit.edu
(defun sje-ignore-buffername-p (bufname)
  "returns T if the buffer should be ignored."
  (let ((data (match-data))
        (re-list sje-switch-buffer-ignore)
        (ignorep nil))
    (while re-list
      (cond
       ((string-match (car re-list) bufname)
        (setq ignorep t)
        (setq re-list nil))
       (t
        (setq re-list (cdr re-list)))))
    (store-match-data data)
    ignorep)
)


(defun next-in-list (elem list)
  "Return the next element after ELEM in LIST.  If ELEM is last in LIST,
return the first element of LIST. If ELEM is not in LIST, return nil"

  (let ( (next nil) (notfound t) returnval (first (car list)) )
    (setq list (append list (list first)))
    ;(message "new list %s" list)))

    (while (and notfound list)
      (setq next (car list))
      (setq list (cdr list))
      (if (eq next elem)
	  (progn
	    (setq notfound nil)
	    (setq returnval (car list)))
	)
      ) ; while
    (if notfound (setq returnval nil))
    
    returnval
))
    

(provide 'sje-switch-buffer)
