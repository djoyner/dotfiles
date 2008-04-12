;;; ~/emacs/lisp/efuncs.el

;; Different platforms use different line endings
(defun unix-file ()
  "Change the current buffer to Latin 1 with Unix line-ends."
  (interactive)
  (set-buffer-file-coding-system 'iso-latin-1-unix t))

(defun dos-file ()
  "Change the current buffer to Latin 1 with DOS line-ends."
  (interactive)
  (set-buffer-file-coding-system 'iso-latin-1-dos t))

(defun mac-file ()
  "Change the current buffer to Latin 1 with Mac line-ends."
  (interactive)
  (set-buffer-file-coding-system 'iso-latin-1-mac t))

;; Support for copying fontified regions to the Windows clipboard
(require 'htmlize)
(and (eq window-system 'w32)
     (defun w32-fontified-region-to-clipboard (start end)
       "Htmlizes region, saves it as a html file, scripts Microsoft Word to
open in the background and to copy all text to the clipboard, then
quits. Useful if you want to send fontified source code snippets to
your friends using RTF-formatted e-mails.

Version: 0.2

Author:

Mathias Dahl, <mathias@cucumber.dahl.net>. Remove the big, green
vegetable from my e-mail address...

Requirements:

* htmlize.el
* wscript.exe must be installed and enabled
* Microsoft Word must be installed

Usage:

Mark a region of fontified text, run this function and in a number of
seconds you have the whole colorful text on your clipboard, ready to
be pasted into a RTF-enabled application.

"
       (interactive "r")
       (let ((snippet (buffer-substring start end))
	     (buf (get-buffer-create "*htmlized_to_clipboard*"))
	     (script-file-name (expand-file-name "~/tmp/htmlized_to_clipboard.vbs"))
	     (htmlized-file-name (expand-file-name "~/tmp/htmlized.html")))
	 (set-buffer buf)
	 (erase-buffer)
	 (insert snippet)
	 (let ((tmp-html-buf (htmlize-buffer)))
	   (set-buffer tmp-html-buf)
	   (write-file htmlized-file-name)
	   (kill-buffer tmp-html-buf))
	 (set-buffer buf)
	 (erase-buffer)
	 (setq htmlized-file-name 
	       (substitute ?\\ ?/ htmlized-file-name))
	 (insert
	  (concat
	   "Set oWord = CreateObject(\"Word.Application\")\n"
	   "oWord.Documents.Open(\"" htmlized-file-name "\")\n"
	   "oWord.Selection.HomeKey 6\n"
	   "oWord.Selection.EndKey 6,1\n"
	   "oWord.Selection.Copy\n"
	   "oWord.Quit\n"
	   "Set oWord = Nothing\n"))
	 (write-file script-file-name)
	 (kill-buffer nil)
	 (setq script-file-name
	       (substitute ?\\ ?/ script-file-name))
	 (w32-shell-execute nil "wscript.exe" 
			    script-file-name))))

;; Clear shell contents
(defun shell-clear-region ()
  (interactive)
  (delete-region (point-min) (point-max))
  (comint-send-input))

;;; end ~/emacs/lisp/efuncs.el
