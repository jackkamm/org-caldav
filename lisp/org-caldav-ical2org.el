;;; org-caldav-ical2org.el --- Import iCalendar to Org  -*- lexical-binding: t; -*-

;; Copyright (C) 2012-2017 Free Software Foundation, Inc.
;; Copyright (C) 2018-2023 David Engster

;; Author: David Engster <deng@randomsample.de>
;; Maintainer: Jack Kamm <jackkamm@tatersworld.org>
;; Keywords: calendar, caldav
;; URL: https://github.com/dengste/org-caldav/
;; Package-Requires: ((emacs "26.3") (org "9.1"))

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

(require 'org-caldav-core)

(defun org-caldav-insert-org-event-or-todo (eventdata-alist)
  "Insert org block from given event data at current position.
Elements of EVENTDATA-ALIST are passed on as arguments to
`org-caldav-insert-org-entry' and `org-caldav-insert-org-todo'.
Returns MD5 from entry."
  (let-alist eventdata-alist
    (if (eq .component-type 'todo)
        (org-caldav-insert-org-todo
         .start-d .start-t .due-d .due-t .priority .percent-complete
         .summary .description .completed-d .completed-t .categories
         .uid .level)
      (org-caldav-insert-org-entry
       .start-d .start-t .end-d .end-t .summary
       .description .location .e-type .uid .level))))

(defun org-caldav--insert-description (description)
  (when (> (length description) 0)
    (when org-caldav-description-blank-line-before (newline))
    (let ((beg (point)))
      (insert description)
      (org-indent-region beg (point)))
    (when org-caldav-description-blank-line-after (newline))
    (newline)))

(defun org-caldav-insert-org-entry (start-d start-t end-d end-t
                                            summary description location e-type
                                            &optional uid level)
  "Insert org block from given data at current position.
START/END-D: Start/End date.  START/END-T: Start/End time.
SUMMARY, DESCRIPTION, LOCATION, UID: obvious.
Dates must be given in a format `org-read-date' can parse.

If LOCATION is \"\", no LOCATION: property is written.
If UID is nil, no UID: property is written.
If LEVEL is nil, it defaults to 1.

Returns MD5 from entry."
  (insert (make-string (or level 1) ?*) " " summary "\n")
  (insert (if org-adapt-indentation "  " "")
   (org-caldav-create-time-range start-d start-t end-d end-t e-type) "\n")
  (org-caldav--insert-description description)
  (forward-line -1)
  (when uid
    (org-set-property "ID" (url-unhex-string uid)))
  (org-caldav-change-location location)
  (org-caldav-insert-org-entry--wrapup))

(defun org-caldav-insert-org-todo (start-d start-t due-d due-t
                                    priority percent-complete
                                    summary description
                                    completed-d completed-t
                                    categories
                                    &optional uid level)
  "Insert org block from given data at current position.
START/DUE-D: Start/Due date.  START/DUE-T: Start/Due time.
PRIORITY: 0-9, PERCENT-COMPLETE: 0-100.
See `org-caldav-todo-priority' and
`org-caldav-todo-percent-states' for explanations how this values
are used.
SUMMARY, DESCRIPTION, UID: obvious.
Dates must be given in a format `org-read-date' can parse.

If UID is nil, no UID: property is written.
If LEVEL is nil, it defaults to 1.

Returns MD5 from entry."
  (let* ((nprio (string-to-number (or priority "0")))
          (r nil)
          (vprio (dolist (p org-caldav-todo-priority r)
                   (when (>= nprio (car p))
                     (setq r (car (cdr p))))))
          (prio (if vprio (concat "[#" vprio "] ") "")))
    (insert (make-string (or level 1) ?*) " "
            (org-caldav--todo-percent-to-state
             (string-to-number (or percent-complete "0")))
            " " prio summary "\n"))
  (org-caldav--insert-description description)
  (forward-line -1)
  (when start-d
    (org--deadline-or-schedule
     nil 'scheduled (org-caldav-convert-to-org-time start-d start-t)))
  (when due-d
    (org--deadline-or-schedule
     nil 'deadline (org-caldav-convert-to-org-time due-d due-t)))
  (when completed-d
    (org-add-planning-info 'closed (org-caldav-convert-to-org-time completed-d completed-t)))
  (org-caldav-set-org-tags categories)
  (when uid (org-set-property "ID" (url-unhex-string uid)))
  (org-caldav-insert-org-entry--wrapup))

;; The following is taken from icalendar.el, written by Ulf Jasper.
;; The LOCATION property is added the extracted list
(defun org-caldav-convert-event-or-todo (is-todo)
  "Convert icalendar event or todo in current buffer.
If IS-TODO, it is a VTODO, else a VEVENT.  Returns an alist of properties
which can be fed into `org-caldav-insert-org-event-or-todo'."
  (let ((decoded (decode-coding-region (point-min) (point-max) 'utf-8 t)))
    (erase-buffer)
    (set-buffer-multibyte t)
    (setq buffer-file-coding-system 'utf-8)
    (insert decoded))
  (goto-char (point-min))
  (let* ((calendar-date-style 'european)
	 (ical-list (icalendar--read-element nil nil))
	 (e (car (if is-todo
                     (org-caldav--icalendar--all-todos ical-list)
                   (icalendar--all-events ical-list))))
	 (zone-map (icalendar--convert-all-timezones ical-list))
         (dtstart-plist (org-caldav--event-date-plist e 'DTSTART zone-map))
         (eventdata-alist
          `((start-d . ,(plist-get dtstart-plist 'date))
            (start-t . ,(plist-get dtstart-plist 'time))
            (dtstart-dec . ,(plist-get dtstart-plist 'decoded))
            (summary . ,(icalendar--convert-string-for-import
		         (or (icalendar--get-event-property e 'SUMMARY)
		             "No Title")))
            (description . ,(icalendar--convert-string-for-import
		             (or (icalendar--get-event-property e 'DESCRIPTION)
			         ""))))))
    (if is-todo
        (org-caldav-convert-event-or-todo--todo e zone-map eventdata-alist)
      (org-caldav-convert-event-or-todo--event e zone-map eventdata-alist))))

(defun org-caldav-convert-event-or-todo--event (e zone-map eventdata-alist)
  "Helper function of `org-caldav-event-or-todo' to handle VEVENT."
  (let* ((start-d (cdr (assq 'start-d eventdata-alist)))
         (start-t (cdr (assq 'start-t eventdata-alist)))
         (dtstart-dec (cdr (assq 'dtstart-dec eventdata-alist)))
         (summary (cdr (assq 'summary eventdata-alist)))
         (dtend-plist (org-caldav--event-date-plist e 'DTEND zone-map))
	 (dtend-dec (plist-get dtend-plist 'decoded))
	 (dtend-1-dec (icalendar--decode-isodatetime
		       (plist-get dtend-plist 'event-property) -1
		       (plist-get dtend-plist 'zone)))
	 e-type
	 (duration (icalendar--get-event-property e 'DURATION)))
    (when (string-match "^\\(?:\\(DL\\|S\\):\s+\\)?\\(.*\\)$" summary)
      (setq e-type (match-string 1 summary))
      (setq summary (match-string 2 summary)))
    (when duration
      (let ((dtend-dec-d (icalendar--add-decoded-times
			  dtstart-dec
			  (icalendar--decode-isoduration duration)))
	    (dtend-1-dec-d (icalendar--add-decoded-times
			    dtstart-dec
			    (icalendar--decode-isoduration duration
							   t))))
	(if (and dtend-dec (not (eq dtend-dec dtend-dec-d)))
	    (message "Inconsistent endtime and duration for %s"
		     summary))
	(setq dtend-dec dtend-dec-d)
	(setq dtend-1-dec dtend-1-dec-d)))
    (let ((end-t (org-caldav--datetime-to-colontime
		  dtend-dec e 'DTEND start-t)))
      ;; Return result
      (append `((component-type . event)
		(end-d
		 . ,(if end-t
			(if dtend-dec
			    (icalendar--datetime-to-diary-date dtend-dec)
			  start-d)
		      (if dtend-1-dec
			  (icalendar--datetime-to-diary-date dtend-1-dec)
			start-d)))
		(end-t . ,end-t)
		(location
		 . ,(icalendar--convert-string-for-import
		     (or (icalendar--get-event-property e 'LOCATION) "")))
		(end-type . ,e-type))
	      eventdata-alist))))

(defun org-caldav-convert-event-or-todo--todo (e zone-map eventdata-alist)
  "Helper function of `org-caldav-event-or-todo' to handle VTODO."
  (let* ((dtdue-plist (org-caldav--event-date-plist e 'DUE zone-map))
	 (dtcomplete-plist (org-caldav--event-date-plist
			    e 'COMPLETED zone-map))
         (percent-complete (icalendar--get-event-property e 'PERCENT-COMPLETE))
         (stat (icalendar--get-event-property e 'STATUS)))
    (unless percent-complete
      (setq percent-complete
        (cond
          ((string= stat "NEEDS-ACTION") "0")
          ((string= stat "IN-PROCESS") "50")
          ((string= stat "COMPLETED") "100")
          (t "0"))))
    (append `((component-type . todo)
	      (due-d . ,(plist-get dtdue-plist 'date))
	      (due-t . ,(plist-get dtdue-plist 'time))
	      (priority . ,(icalendar--get-event-property e 'PRIORITY))
              (percent-complete ., percent-complete)
	      (status . ,stat)
	      (completed-d . ,(plist-get dtcomplete-plist 'date))
	      (completed-t . ,(plist-get dtcomplete-plist 'time))
              (categories . ,(icalendar--convert-string-for-import
                              (or (icalendar--get-event-property e 'CATEGORIES)
                                  ""))))
	    eventdata-alist)))

;; TODO: org-caldav-ical2org function that converts ics to a separate
;; org file, or adds to an arbitrary file (rather than adding contents
;; to org-caldav-inbox)

;; Not sure how the below functions are used. Maybe they're supposed
;; to import ics events from email? Here's the PR where it was added,
;; not much info unfortunately:
;; https://github.com/dengste/org-caldav/pull/101

;;;###autoload
(defun org-caldav-import-ics-buffer-to-org ()
  "Add ics content in current buffer to `org-caldav-inbox'."
  (let ((event (org-caldav-convert-event-or-todo nil))
        (file (org-caldav-inbox-file org-caldav-inbox)))
    (with-current-buffer (find-file-noselect file)
      (let* ((point-and-level (org-caldav-inbox-point-and-level
                               org-caldav-inbox event))
             (point (car point-and-level))
             (level (cdr point-and-level)))
        (goto-char point)
        (org-caldav-insert-org-event-or-todo
         (append event `((uid . nil) (level . ,level))))
        (message "%s: Added event: %s"
                 file
                 (buffer-substring
                  point
                  (save-excursion
                    (goto-char point)
                    (point-at-eol 2))))))))

;;;###autoload
(defun org-caldav-import-ics-to-org (path)
  "Add ics content in PATH to `org-caldav-inbox'."
  (with-current-buffer (get-buffer-create "*import-ics-to-org*")
    (delete-region (point-min) (point-max))
    (insert-file-contents path)
    (org-caldav-import-ics-buffer-to-org)))


(provide 'org-caldav-ical2org)
;;; org-caldav-ical2org.el ends here
