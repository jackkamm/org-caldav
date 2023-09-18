;;; org-caldav-core.el --- Core functions for org-caldav  -*- lexical-binding: t; -*-

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

;;; Code:

(defcustom org-caldav-select-tags nil
  "List of tags to filter the synced tasks.
If any such tag is found in a buffer, all items that do not carry
one of these tags will not be exported."
  :type '(repeat string))

(defcustom org-caldav-exclude-tags nil
  "List of tags to exclude from the synced tasks.
All items that carry one of these tags will not be exported."
  :type '(repeat string))

(defcustom org-caldav-days-in-past nil
  "Number of days before today to skip in the exported calendar.
This makes it very easy to keep the remote calendar clean.

nil means include all entries (default)
any number set will cut the dates older than N days in the past."
  :type 'integer)

(defcustom org-caldav-skip-conditions nil
  "Conditions for skipping entries during icalendar export.
This must be a list of conditions, which are described in the
doc-string of `org-agenda-skip-if'.  Any entry that matches will
not be exported.  Note that the normal `org-agenda-skip-function'
has no effect on the icalendar exporter."
  :type 'list)

(defcustom org-caldav-todo-deadline-schedule-warning-days nil
  "Whether to auto-create SCHEDULED timestamp from DEADLINE.

When set to `t', on sync any TODO item with a DEADLINE timestamp
will have a SCHEDULED timestamp added if it doesn't already have
one.

This uses the warning string like DEADLINE: <2017-07-05 Wed -3d>
to a SCHEDULED <2017-07-02 Sun>.  If the warning days (here -3d)
is not given it is taken from `org-deadline-warning-days'.

This might be useful for OpenTasks users, to prevent the app from
showing tasks which have a deadline years in the future."
  :type 'boolean)

(defmacro org-caldav--suppress-obsolete-warning (var body)
  "Macro for compatibility.
To be removed when emacs dependency reaches >=27.1."
  (declare (indent defun))
  (if (fboundp 'with-suppressed-warnings)
      `(with-suppressed-warnings ((obsolete ,var))
         ,body))
  `(with-no-warnings ,body))

(defun org-caldav-get-org-files-for-sync ()
  "Return list of all org files for syncing.
This adds the inbox if necessary."
  (let ((inbox (org-caldav-inbox-file org-caldav-inbox)))
    (append org-caldav-files
	    (when (and inbox
		       (org-caldav-sync-do-org->cal)
		       (not (member inbox org-caldav-files)))
	      (list inbox)))))

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

(provide 'org-caldav-core)
;;; org-caldav-core.el ends here
