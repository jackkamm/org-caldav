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

;; TODO

;;; Code:

(require 'ox-icalendar)
(require 'org-caldav-core)

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
