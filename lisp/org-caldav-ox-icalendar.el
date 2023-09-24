;;; org-caldav-ox-icalendar.el --- Export Org to iCalendar  -*- lexical-binding: t; -*-

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

;; TODO Rename this to org-caldav-org2ics.el

;;; Code:

(require 'ox-icalendar)
(require 'org-caldav-core)

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

(defun org-caldav-get-org-files-for-sync ()
  "Return list of all org files for syncing.
This adds the inbox if necessary."
  (let ((inbox (org-caldav-inbox-file org-caldav-inbox)))
    (append org-caldav-files
	    (when (and inbox
		       (org-caldav-sync-do-org->cal)
		       (not (member inbox org-caldav-files)))
	      (list inbox)))))

(defun org-caldav-convert-buffer-to-crlf ()
  "Converts local buffer to the dos format using crlf at the end
  of the line.  Some ical validators fail otherwise."
  (save-excursion
    (goto-char (point-min))
    (while (not (= (point) (point-max)))
      (goto-char (- (point-at-eol) 1))
      (unless (string= (thing-at-point 'char) "\^M")
        (forward-char)
        (insert "\^M"))
      (forward-line))))

(defun org-caldav-cleanup-ics-description ()
  "Cleanup description for event in current buffer.
This removes an initial timestamp or range if it wasn't removed
by ox-icalendar."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward
           ;; Can't use org-tsr-regexp because -- is converted to
           ;; unicode emdash –
           (concat "^DESCRIPTION:\\(\\s-*"
                   org-ts-regexp
                   "\\(–"
                   org-ts-regexp
                   "\\)?\\(\\\\n\\\\n\\)?\\)")
            nil t)
      (replace-match "" nil nil nil 1))))

(defun org-caldav-maybe-fix-timezone ()
  "Fix the timezone if it is all uppercase.
This is a bug in older Org versions."
  (unless (null org-icalendar-timezone)
    (save-excursion
      (goto-char (point-min))
      (while (search-forward (upcase org-icalendar-timezone) nil t)
        (replace-match org-icalendar-timezone t)))))

(defun org-caldav-fix-todo-priority ()
  "icalendar exports default priority with ical export.  We want
  a priority of 0 if is not set."
  (save-excursion
    (goto-char (point-min))
    (when (search-forward "BEGIN:VTODO" nil t)
      (search-forward "PRIORITY:")
      (unless (eq (thing-at-point 'number) 0)
        ;; NOTE: Deletion up to eol-1 assumes the line ends with ^M
        (delete-region (point) (- (point-at-eol) 1))
        (insert (number-to-string
                  (save-excursion
                    (goto-char (point-min))
                    (org-id-goto (org-caldav-get-uid))
                    (org-narrow-to-subtree)
                    (let ((nprio (if (re-search-forward org-priority-regexp nil t)
                                     (let* ((prio (org-entry-get nil "PRIORITY"))
                                            (r 0))
                                       (dolist (pri org-caldav-todo-priority r)
                                         (when (string= (car (cdr pri)) prio)
                                           (setq r (car pri))))
                                       r)
                                   0)))
                      (widen)
                      nprio))))))))

(defun org-caldav-fix-todo-status-percent-state ()
  "icalendar exports only sets the STATUS but not the
PERCENT-COMPLETE.  This works great if you have only TODO and
DONE, but I like to use other states like STARTED or NEXT to
indicate the process.  This fixes the ical values for that.

TODO: save percent-complete also as a property in org"
  (save-excursion
    (goto-char (point-min))
    (when (search-forward "BEGIN:VTODO" nil t)
      (if (search-forward "STATUS:" nil t)
        (delete-region (point-at-bol) (+ 1 (point-at-eol)))
        (progn (search-forward "END:VTODO")
          (goto-char (point-at-bol))))


      (let* ((state (save-excursion
                      (goto-char (point-min))
                      (org-id-goto (org-caldav-get-uid))
                      (substring-no-properties (org-get-todo-state))))
             (r nil)
             (percent (dolist (p org-caldav-todo-percent-states r)
                        (when (string= state (car (cdr p)))
                          (setq r (car p)))))
             (status (if r
                         (cond ((= percent 0) "NEEDS-ACTION")
                               ((= percent 100) "COMPLETED")
                               (t "IN-PROCESS"))
                       (error "Error setting percent state: '%s' not present in org-caldav-todo-percent-states" state)))
             (completed (save-excursion
                          (goto-char (point-min))
                          (org-id-goto (org-caldav-get-uid))
                          (org-element-property :closed (org-element-at-point)))))
        (insert "PERCENT-COMPLETE:" (number-to-string percent) "\n")
        (insert "STATUS:" status "\n")
        ;; if closed missing but in DONE state:
        (when (and (= percent 100) (not completed))
          (setq completed (save-excursion
                            (goto-char (point-min))
                            (org-id-goto (org-caldav-get-uid))
                            (org-add-planning-info 'closed (org-current-effective-time))
                            (org-element-property :closed (org-element-at-point)))))
        (when completed
          (insert (org-icalendar-convert-timestamp
                    completed "COMPLETED") "\n"))))))

(defun org-caldav-fix-categories ()
  "Nextcloud creates an empty category if this is set without any
  entry.  We fix this by removing the CATEGORIES entry."
  (save-excursion
    (goto-char (point-min))
    (when (and (search-forward "CATEGORIES:" nil t)
            (not (thing-at-point 'word)))
      (delete-region (point-at-bol) (+ (point-at-eol) 1)))))

(defun org-caldav-fix-todo-dtstart ()
  "ox-icalendar includes the actual time as DTSTART into the
vtodo.  For nextcloud this behaviour is undesired, because
dtstart is used for the beginning of the task, which is in the
SCHEDULED of the org entry.  Lets see if the org entry has a
scheduled time and remove dtstart if it doesn't.

If `org-caldav-todo-deadline-schedule-warning-days' is set, this will
also look if there is a deadline."
  (save-excursion
    (goto-char (point-min))
    (when (search-forward "BEGIN:VTODO" nil t)
      (when
        (search-forward "DTSTART" nil t)
        (unless
          (save-excursion
            (goto-char (point-min))
            (org-id-goto (org-caldav-get-uid))
            (if (and org-caldav-todo-deadline-schedule-warning-days
                     ;; has deadline warning days set too:
                     (string-match "-\\([0-9]+\\)\\([hdwmy]\\)\\(\\'\\|>\\| \\)"
                                   (or (org-entry-get nil "DEADLINE" nil) "")))
                (or (org-get-scheduled-time nil) (org-get-deadline-time nil))
              (org-get-scheduled-time nil)))
          (delete-region (point-at-bol) (+ 1 (point-at-eol))))))))

(defun org-caldav-skip-function (backend)
  (org-caldav-debug-print 2 "Skipping over excluded entries")
  (when (eq backend 'icalendar)
    (org-map-entries
     (lambda ()
       (let ((pt (save-excursion (apply 'org-agenda-skip-entry-if org-caldav-skip-conditions)))
              (ts (when org-caldav-days-in-past (* (abs org-caldav-days-in-past) -1)))
              (stamp (or (org-entry-get nil "TIMESTAMP" t) (org-entry-get nil "CLOSED" t))))
	 (when (or pt (and stamp ts (> ts (org-time-stamp-to-now stamp))))
           (delete-region (point) (org-end-of-subtree t t))
           (setq org-map-continue-from (point)))))))
  (org-caldav-debug-print 2 "Finished skipping"))

(defun org-caldav-prepare-scheduled-deadline-timestamps (orgfiles)
  "For nextcloud (or maybe the ical standard?) in vtodo the
scheduled and deadline have all have a time specified or none of
them.  So we find todo items which have deadline and scheduled
specified, but one of them has and the other do not have any
time, and we ask the user to fix that."
  (org-map-entries
    (lambda ()
      (let ((sched (org-entry-get nil "SCHEDULED"))
             (deadl (org-entry-get nil "DEADLINE"))
             kchoice)
        (when (and sched deadl)
          (when (and (org-caldav-timestamp-has-time-p sched)
                  (not (org-caldav-timestamp-has-time-p deadl)))
            (org-id-goto (org-id-get-create))
            (setq kchoice (read-char-choice "Scheduled and Deadline
            set.  For syncing you need to (s) set time on
            DEADLINE, or (d) delete SCHEDULED time."
                        '(?s ?d)))
            (cond ((= kchoice ?s) (org-deadline nil))
              ((= kchoice ?d) (org--deadline-or-schedule nil 'scheduled
                                (replace-regexp-in-string " [0-2][0-9]:[0-5][0-9]" "" sched)))))
          (when (and (not (org-caldav-timestamp-has-time-p sched))
                  (org-caldav-timestamp-has-time-p deadl))
            (org-id-goto (org-entry-get nil "ID"))
            (setq kchoice (read-char-choice "Scheduled and Deadline
            set.  For syncing you need to (s) set time on
            SCHEDULED, or (d) delete DEADLINE time."
                        '(?s ?d)))
            (cond ((= kchoice ?s) (org-schedule nil))
              ((= kchoice ?d) (org--deadline-or-schedule nil 'deadline
                                (replace-regexp-in-string " [0-2][0-9]:[0-5][0-9]" "" sched))))))))
    nil orgfiles))

(defun org-caldav-create-uid (file &optional bell)
  "Set ID property on headlines missing it in FILE.
When optional argument BELL is non-nil, inform the user with
a message if the file was modified. This func is the same as
org-icalendar-create-uid except that it ignores entries that
match org-caldav-skip-conditions."
  (let (modified-flag)
    (org-map-entries
     (lambda ()
       (let ((entry (org-element-at-point)))
         (unless (org-element-property :ID entry)
           (unless (apply 'org-agenda-skip-entry-if org-caldav-skip-conditions)
             (org-id-get-create)
             (setq modified-flag t)
             (forward-line)))))
     nil nil 'comment)
    (when (and bell modified-flag)
      (message "ID properties created in file \"%s\"" file)
      (sit-for 2))))

(defun org-caldav-generate-ics ()
  "Generate ICS file from `org-caldav-files'.
Returns buffer containing the ICS file."
  (let ((icalendar-file
	 (if (featurep 'ox-icalendar)
	     'org-icalendar-combined-agenda-file
	   'org-combined-agenda-icalendar-file))
	(orgfiles (org-caldav-get-org-files-for-sync))
	(org-export-select-tags org-caldav-select-tags)
	(org-icalendar-exclude-tags org-caldav-exclude-tags)
        ;; We create UIDs ourselves and do not rely on ox-icalendar.el
	(org-icalendar-store-UID nil)
	;; Does not work yet
	(org-icalendar-include-bbdb-anniversaries nil)
	(icalendar-uid-format "orgsexp-%h")
	(org-icalendar-date-time-format
	 (cond
	  ((and org-icalendar-timezone
		(string= org-icalendar-timezone "UTC"))
	   ":%Y%m%dT%H%M%SZ")
	  (org-icalendar-timezone
	   ";TZID=%Z:%Y%m%dT%H%M%S")
	  (t
	   ":%Y%m%dT%H%M%S"))))
    (dolist (orgfile orgfiles)
      (with-current-buffer (org-get-agenda-file-buffer orgfile)
        (org-caldav-create-uid orgfile t)))
    ;; check scheduled and deadline for having both time or none (vtodo)
    (org-caldav-prepare-scheduled-deadline-timestamps orgfiles)
    (set icalendar-file (make-temp-file "org-caldav-"))
    (org-caldav-debug-print 1 (format "Generating ICS file %s."
				      (symbol-value icalendar-file)))
    ;; compat: use org-export-before-parsing-functions after org >=9.6
    (org-caldav--suppress-obsolete-warning org-export-before-parsing-hook
      (let ((org-export-before-parsing-hook
	     (append org-export-before-parsing-hook
                     (when (or org-caldav-skip-conditions
                               org-caldav-days-in-past)
                       '(org-caldav-skip-function))
                     (when org-caldav-todo-deadline-schedule-warning-days
                       '(org-caldav-scheduled-from-deadline)))))
        ;; Export events to one single ICS file.
        (apply 'org-icalendar--combine-files orgfiles)))
    (find-file-noselect (symbol-value icalendar-file))))

(defun org-caldav-rewrite-uid-in-event ()
  "Rewrite UID in narrowed vevent buffer.
This will strip prefixes like 'DL' or 'TS' the Org exporter puts
in the UID and also remove whitespaces. Throws an error if there
is no UID to rewrite. Returns the UID."
  (save-excursion
    (goto-char (point-min))
    (let ((uid (org-caldav-get-uid)))
      (when uid
	(goto-char (point-min))
	(re-search-forward "^UID:")
	(let ((pos (point)))
	  (while (progn (forward-line)
			(looking-at " \\(.+\\)\\s-*$")))
	  (delete-region pos (point)))
	(insert uid "\n"))
      uid)))

(defun org-caldav-patch-ics--uid (buf)
  "Rewrite all UIDs in ICS buffer BUF.
This will strip prefixes like 'DL' or 'TS' the Org exporter puts
in the UID and also remove whitespaces."
  (with-current-buffer buf
    (goto-char (point-min))
    (while (org-caldav-narrow-next-event)
      (org-caldav-rewrite-uid-in-event))))

(defun org-caldav-patch-ics--rest (buf)
  "Apply fixes to exported ics from ox-icalendar, aside from uid fixes.
The uid is patched separately in `org-caldav-patch-ics--uid',
because it needs to happen before the md5sum for
back-compatibility reasons."
  (with-current-buffer buf
    (org-caldav-convert-buffer-to-crlf)
    (goto-char (point-min))
    (while (org-caldav-narrow-next-event)
      (org-caldav-cleanup-ics-description)
      (org-caldav-maybe-fix-timezone)
      (org-caldav-fix-todo-priority)
      (org-caldav-fix-todo-status-percent-state)
      (org-caldav-fix-categories)
      (org-caldav-fix-todo-dtstart))))

(defun org-caldav-patch-ics--all (icsbuf)
  "Apply fixes to exported ics from ox-icalendar."
  (org-caldav-patch-ics--uid icsbuf)
  (org-caldav-patch-ics--rest icsbuf)
  icsbuf)

(defun org-caldav-export-ics ()
  "Generate ICS file from `org-caldav-files'.
Returns buffer containing the ICS file."
  (interactive)
  (org-caldav-patch-ics--all (org-caldav-generate-ics)))

(defun org-caldav-scheduled-from-deadline (backend)
  "Create a scheduled entry from deadline."
  (when (eq backend 'icalendar)
    (org-map-entries
     (lambda ()
       (let* ((sched (org-element-property :scheduled (org-element-at-point)))
              (ts (org-element-property :deadline (org-element-at-point)))
              (raw (org-element-property :raw-value ts))
              (wu (org-element-property :warning-unit ts))
              (wv (org-element-property :warning-value ts))
              (dip (when org-caldav-days-in-past (* (abs org-caldav-days-in-past) -1)))
              (stamp (org-entry-get nil "DEADLINE")))
         ;; skip if too old:
         (unless (and dip stamp (> dip (org-time-stamp-to-now stamp)))
         (when (and ts (not sched))
           (org--deadline-or-schedule nil 'scheduled raw)
           (search-forward "SCHEDULED: ")
           (forward-char)
           (if wv
               (progn
                 (cond ((eq wu 'week) (setq wu 'day wv (* wv 7)))
                       ((eq wu 'hour) (setq wu 'minute wv (* wv 60))))
                 (org-timestamp-change (* wv -1) wu))
               (org-timestamp-change (* org-deadline-warning-days -1) 'day))))
         (org-back-to-heading)
         (org-caldav-debug-print 2 (format "scheduled: %s" (org-entry-get nil
                                                                 "SCHEDULED" t))))))))

(provide 'org-caldav-ox-icalendar)
;;; org-caldav-ox-icalendar.el ends here
