;;; funcs.el --- djoyner-org-mode layer funcs file for Spacemacs.
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
;;
;; Author: Joyner <djoyner@RTPC02CC56RMD6N>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'org)

;; Adapted from https://github.com/alphapapa/unpackaged.el
(defun djoyner-org-mode/org-fix-blank-lines (&optional prefix)
  "Ensure that blank lines exist between headings and between headings and their contents.
With prefix, operate on whole buffer. Ensures that blank lines
exist after each headings's drawers."
  (interactive "P")
  (org-map-entries (lambda ()
                     (org-with-wide-buffer
                      ;; `org-map-entries' narrows the buffer, which prevents us from seeing
                      ;; newlines before the current heading, so we do this part widened.
                      (while (not (looking-back "\n\n" nil))
                        ;; Insert blank lines before heading.
                        (insert "\n")))
                     (let ((end (org-entry-end-position)))
                       ;; Insert blank lines before entry content
                       (forward-line)
                       (while (and (org-at-planning-p)
                                   (< (point) (point-max)))
                         ;; Skip planning lines
                         (forward-line))
                       (while (re-search-forward org-drawer-regexp end t)
                         ;; Skip drawers. You might think that `org-at-drawer-p' would suffice, but
                         ;; for some reason it doesn't work correctly when operating on hidden text.
                         ;; This works, taken from `org-agenda-get-some-entry-text'.
                         (re-search-forward "^[ \t]*:END:.*\n?" end t)
                         (goto-char (match-end 0)))
                       (unless (or (= (point) (point-max))
                                   (org-at-heading-p)
                                   (looking-at-p "\n"))
                         (insert "\n"))))
                   t (if prefix
                         nil
                       'tree)))

(defun djoyner-org-mode/org-fix-blank-lines-whole-buffer ()
  "Ensure that blank lines exist between headings and between headings and their contents.
Operates on whole buffer. Ensures that blank lines exist after
each headings's drawers."
  (interactive)
  (let ((current-prefix-arg t))
    (djoyner-org-mode/org-fix-blank-lines))
)

;; Adapted from Rainer König's Udemy course materials
(defun djoyner-org-mode/org-reset-checkbox-state-maybe ()
  "Reset all checkboxes in an entry if the `RESET_CHECKBOXES' property is set"
  (interactive "*")
  (if (org-entry-get (point) "RESET_CHECKBOXES")
      (org-reset-checkbox-state-subtree)))

(defun djoyner-org-mode/org-checklist ()
  (when (member org-state org-done-keywords) ;; org-state dynamically bound in org.el/org-todo
    (djoyner-org-mode/org-reset-checkbox-state-maybe)))
