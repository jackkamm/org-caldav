;;; org-caldav-eventdb.el --- Eventdb for org-caldav  -*- lexical-binding: t; -*-

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

(defvar org-caldav-event-list nil
  "The event list database.
This is an alist with elements
  (uid md5 etag sequence status).
It will be saved to disk between sessions.")

(defsubst org-caldav-add-event (uid md5 etag sequence status)
  "Add event with UID, MD5, ETAG and STATUS."
  (setq org-caldav-event-list
	(append org-caldav-event-list
		(list (list uid md5 etag sequence status)))))

(defsubst org-caldav-search-event (uid)
  "Return entry with UID from even list."
  (assoc uid org-caldav-event-list))

(defun org-caldav-filter-events (status)
  "Return list of events with STATUS."
  (delq nil
	(mapcar
	 (lambda (event)
	   (when (eq (car (last event)) status)
	     event))
	 org-caldav-event-list)))

(provide 'org-caldav-eventdb)
;;; org-caldav-eventdb.el ends here
