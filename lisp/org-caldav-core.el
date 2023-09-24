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
;; TODO Rename this to org-caldav-utils?

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

(defcustom org-caldav-todo-percent-states '((0 "TODO") (100 "DONE"))
  "Mapping between `org-todo-keywords' & iCal VTODO's percent-complete.

iCalendar's percent-complete is a positive integer between 0 and
100. The default value for `org-caldav-todo-percent-states' maps
these to `org-todo-keywords' as follows: 0-99 is TODO, and 100 is
DONE.

The following example would instead map 0 to TODO, 1 to NEXT,
2-99 to PROG, and 100 to DONE:

  (setq org-caldav-todo-percent-states
        '((0 \"TODO\") (1 \"NEXT\") (2 \"PROG\") (100 \"DONE\")))

Note: You should check that the keywords in
`org-caldav-todo-percent-states' are also valid keywords in
`org-todo-keywords'."
  :type 'list)

(defmacro org-caldav--suppress-obsolete-warning (var body)
  "Macro for compatibility.
To be removed when emacs dependency reaches >=27.1."
  (declare (indent defun))
  (if (fboundp 'with-suppressed-warnings)
      `(with-suppressed-warnings ((obsolete ,var))
         ,body))
  `(with-no-warnings ,body))

(defsubst org-caldav-event-md5 (event)
  "Get MD5 from EVENT."
  (nth 1 event))

(defsubst org-caldav-event-etag (event)
  "Get etag from EVENT."
  (nth 2 event))

(defsubst org-caldav-event-sequence (event)
  "Get sequence number from EVENT."
  (nth 3 event))

(defsubst org-caldav-event-status (event)
  "Get status from EVENT."
  (nth 4 event))

(defsubst org-caldav-event-set-status (event status)
  "Set status from EVENT to STATUS."
  (setcar (last event) status))

(defsubst org-caldav-event-set-etag (event etag)
  "Set etag from EVENT to ETAG."
  (setcar (nthcdr 2 event) etag))

(defsubst org-caldav-event-set-md5 (event md5sum)
  "Set md5 from EVENT to MD5SUM."
  (setcar (cdr event) md5sum))

(defsubst org-caldav-event-set-sequence (event seqnum)
  "Set sequence number from EVENT to SEQNUM."
  (setcar (nthcdr 3 event) seqnum))

(defsubst org-caldav-use-oauth2 ()
  (symbolp org-caldav-url))

(defun org-caldav-get-event (uid &optional with-headers)
  "Get event with UID from calendar.
Function returns a buffer containing the event, or nil if there's
no such event.
If WITH-HEADERS is non-nil, do not delete headers.
If retrieve fails, do `org-caldav-retry-attempts' retries."
  (org-caldav-debug-print 1 (format "Getting event UID %s." uid))
  (let ((counter 0)
	eventbuffer errormessage)
    (while (and (not eventbuffer)
		(< counter org-caldav-retry-attempts))
      (with-current-buffer
	  (org-caldav-url-retrieve-synchronously
	   (concat (org-caldav-events-url) (url-hexify-string uid) org-caldav-uuid-extension))
	(goto-char (point-min))
	(if (looking-at "HTTP.*2[0-9][0-9]")
	    (setq eventbuffer (current-buffer))
	  ;; There was an error retrieving the event
	  (setq errormessage (buffer-substring (point-min) (point-at-eol)))
	  (setq counter (1+ counter))
	  (org-caldav-debug-print
	   1 (format "(Try %d) Error when trying to retrieve UID %s: %s"
		     counter uid errormessage)))))
    (unless eventbuffer
      ;; Give up
      (error "Failed to retrieve UID %s after %d tries with error %s"
	     uid org-caldav-retry-attempts errormessage))
    (with-current-buffer eventbuffer
      (unless (search-forward "BEGIN:VCALENDAR" nil t)
	(error "Failed to find calendar entry for UID %s (see buffer %s)"
	       uid (buffer-name eventbuffer)))
      (beginning-of-line)
      (unless with-headers
	(delete-region (point-min) (point)))
      (save-excursion
	(while (re-search-forward "\^M" nil t)
	  (replace-match "")))
      ;; Join lines because of bug in icalendar parsing.
      (save-excursion
	(while (re-search-forward "^ " nil t)
	  (delete-char -2)))
      (org-caldav-debug-print 2 (format "Content of event UID %s: " uid)
			      (buffer-string)))
    eventbuffer))

(defun org-caldav-get-org-files-for-sync ()
  "Return list of all org files for syncing.
This adds the inbox if necessary."
  (let ((inbox (org-caldav-inbox-file org-caldav-inbox)))
    (append org-caldav-files
	    (when (and inbox
		       (org-caldav-sync-do-org->cal)
		       (not (member inbox org-caldav-files)))
	      (list inbox)))))

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

(provide 'org-caldav-core)
;;; org-caldav-core.el ends here
