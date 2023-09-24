;;; org-caldav-utils.el --- Core functions for org-caldav  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Jack Kamm

;; Author: Jack Kamm <jackkamm@gmail.com>
;; Keywords: calendar

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; TODO
;; TODO Rename this to org-caldav-utils?

;;; Code:

(defcustom org-caldav-todo-priority '((0 nil) (1 "A") (5 "B") (9 "C"))
  "Mapping between iCalendar and Org TODO priority levels.

The iCalendar priority is an integer 1-9, with lower number
having higher priority, and 0 equal to unspecified priority. The
default Org priorities are A-C, but this can be changed with
`org-priority-highest' and `org-priority-lowest'. If you change
the default Org priority, you should also update this
variable (`org-caldav-todo-priority').

The default mapping is: 0 is no priority, 1-4 is #A, 5-8 is #B,
and 9 is #C.

TODO: Store the priority in a property and sync it."
  :type 'list)

(defmacro org-caldav--suppress-obsolete-warning (var body)
  "Macro for compatibility.
To be removed when emacs dependency reaches >=27.1."
  (declare (indent defun))
  (if (fboundp 'with-suppressed-warnings)
      `(with-suppressed-warnings ((obsolete ,var))
         ,body))
  `(with-no-warnings ,body))

(defun org-caldav-get-uid ()
  "Get UID for event in current buffer."
  (if (re-search-forward "^UID:\\s-*\\(.+\\)\\s-*$" nil t)
      (let ((case-fold-search nil)
            (uid (match-string 1)))
	(while (progn (forward-line)
		      (looking-at " \\(.+\\)\\s-*$"))
	  (setq uid (concat uid (match-string 1))))
	(while (string-match "\\s-+" uid)
	  (setq uid (replace-match "" nil nil uid)))
        (when (string-match "^\\(\\(DL\\|SC\\|TS\\|TODO\\)[0-9]*-\\)" uid)
	  (setq uid (replace-match "" nil nil uid)))
	uid)
    (error "No UID could be found for current event.")))

(defun org-caldav-debug-print (level &rest objects)
  "Print OBJECTS into debug buffer with debug level LEVEL.
Do nothing if LEVEL is larger than `org-caldav-debug-level'."
  (unless (or (null org-caldav-debug-level)
	      (> level org-caldav-debug-level))
    (with-current-buffer (get-buffer-create org-caldav-debug-buffer)
      (dolist (cur objects)
	(if (stringp cur)
	    (insert cur)
	  (prin1 cur (current-buffer)))
	(insert "\n")))))

(defun org-caldav-narrow-next-event ()
  "Narrow next event in the current buffer.
If buffer is currently not narrowed, narrow to the first one.
Returns nil if there are no more events."
  (if (not (org-caldav-buffer-narrowed-p))
      (goto-char (point-min))
    (goto-char (point-max))
    (widen))
  (if (null (re-search-forward "BEGIN:V[EVENT|TODO]" nil t))
      (progn
	;; No more events.
	(widen)	nil)
    (beginning-of-line)
    (narrow-to-region (point)
		      (save-excursion
                        (re-search-forward "END:V[EVENT|TODO]")
			(forward-line 1)
			(point)))
    t))

(provide 'org-caldav-utils)
;;; org-caldav-utils.el ends here
