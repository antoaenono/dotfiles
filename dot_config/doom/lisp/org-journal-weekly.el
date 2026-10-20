;;; org-journal-weekly.el --- Roam dailies to weekly drafts -*- lexical-binding: t; -*-

(require 'org-roam-dailies)
(require 'cal-iso)
(require 'seq)

(defvar my/org-weekly-do-directory "~/org/time/week/do/"
  "Directory for editable weekly Do drafts.")

(defun my/org-journal--daily-body (file)
  "Copy the body of daily FILE for a weekly draft without changing its source.
Include unsaved edits.  Remove file metadata and copied global IDs, resolve
relative file links, and nest the daily note's headings below its date."
  (with-temp-buffer
    (let ((source (find-buffer-visiting file)))
      (if source
          (insert (with-current-buffer source
                    (save-restriction
                      (widen)
                      (buffer-substring-no-properties (point-min) (point-max)))))
        (insert-file-contents file)))
    (delay-mode-hooks (org-mode))
    (goto-char (point-min))
    (when (looking-at-p org-property-drawer-re)
      (org-roam-end-of-meta-data)
      (delete-region (point-min) (point)))
    (dolist (keyword (reverse (org-element-map (org-element-parse-buffer)
                                 'keyword #'identity)))
      (when (and (not (org-element-lineage keyword '(headline)))
                 (member (org-element-property :key keyword)
                         '("TITLE" "DATE" "FILETAGS" "STARTUP" "PUBLISH" "DRAFT"
                           "ROAM_ALIASES" "ROAM_REFS")))
        (delete-region (org-element-property :begin keyword)
                       (org-element-property :end keyword))))
    (let ((org-odd-levels-only nil)
          (org-adapt-indentation nil))
      (org-map-entries (lambda () (org-entry-delete nil "ID")))
      ;; An unfinished, empty capture does not make a day nonempty.
      (dolist (heading (reverse (org-element-map (org-element-parse-buffer)
                                   'headline #'identity)))
        (when (and (string-empty-p (org-element-property :raw-value heading))
                   (not (org-element-property :todo-keyword heading))
                   (not (org-element-contents heading)))
          (delete-region (org-element-property :begin heading)
                         (org-element-property :end heading))))
      (org-map-entries #'org-demote))
    ;; Parse links so code and examples are left alone.  Edit from the end to
    ;; keep earlier element positions valid.
    (dolist (link (reverse (org-element-map (org-element-parse-buffer) 'link #'identity)))
      (let ((path (org-element-property :path link)))
        (when (and (equal (org-element-property :type link) "file")
                   (not (file-name-absolute-p path)))
          (let* ((begin (org-element-property :begin link))
                 (end (- (org-element-property :end link)
                         (org-element-property :post-blank link)))
                 (description-begin (org-element-property :contents-begin link))
                 (description (and description-begin
                                   (buffer-substring-no-properties
                                    description-begin
                                    (org-element-property :contents-end link))))
                 (application (org-element-property :application link))
                 (search (org-element-property :search-option link))
                 (target (concat "file" (and application (concat "+" application))
                                 ":" (expand-file-name path (file-name-directory file))
                                 (and search (concat "::" search)))))
            (delete-region begin end)
            (goto-char begin)
            (insert (org-link-make-string target description))))))
    (buffer-string)))

(defun my/org-journal--monday (day)
  "Return the Monday of the ISO week containing absolute calendar DAY."
  ;; Calendar represents Sunday as zero, so wrap it back to Monday.
  (- day (mod (1- (nth 1 (calendar-iso-from-absolute day))) 7)))

(defun my/org-journal--weeks (current-monday)
  "Return nonempty, completed Roam daily notes grouped into weekly bodies.
Use Roam's daily directory and date parser.  Exclude CURRENT-MONDAY and
later weeks.  Read visiting buffers without modifying or saving them."
  (let ((directory (expand-file-name org-roam-dailies-directory org-roam-directory))
        entries)
    (when (file-directory-p directory)
      (dolist (file (org-roam-dailies--list-files))
        (let ((date (org-roam-dailies-calendar--file-to-date file)))
          (when (and date (seq-every-p #'integerp date)
                     (calendar-date-is-valid-p date))
            (let ((day (calendar-absolute-from-gregorian date)))
              (when (< day current-monday)
                (let ((body (my/org-journal--daily-body file)))
                  (when (string-match-p "\\S-" body)
                    (push (cons day
                                (concat (format-time-string
                                         "* %Y-%m-%d %A\n"
                                         (encode-time 0 0 12 (nth 1 date) (car date)
                                                      (nth 2 date)))
                                        body "\n"))
                          entries)))))))))
    (setq entries (sort entries (lambda (a b) (< (car a) (car b)))))
    (mapcar (lambda (week)
              (cons (car week) (mapconcat #'cdr (cdr week) "\n")))
            (sort (seq-group-by (lambda (entry) (my/org-journal--monday (car entry)))
                                entries)
                  (lambda (a b) (< (car a) (car b)))))))

(defun my/org-journal--write-week (monday body)
  "Write a missing weekly draft for MONDAY from BODY and return its path.
Never replace an existing file or visiting buffer."
  (let* ((iso (calendar-iso-from-absolute monday))
         (week (format "%d-W%02d" (nth 2 iso) (car iso)))
         (destination (expand-file-name (concat week ".org")
                                        my/org-weekly-do-directory)))
    (unless (or (file-exists-p destination) (find-buffer-visiting destination))
      (let ((start (calendar-gregorian-from-absolute monday)))
        (with-temp-buffer
          (insert (format "#+TITLE: Week %d, %d\n#+DATE: <%04d-%02d-%02d>\n#+PUBLISH: time\n#+DRAFT: true\n\n"
                          (car iso) (nth 2 iso) (nth 2 start) (car start) (nth 1 start))
                  body)
          (unless (bolp) (insert "\n"))
          (make-directory (file-name-directory destination) t)
          ;; Refuse to overwrite even if another process creates it meanwhile.
          (write-region (point-min) (point-max) destination nil 'silent nil 'excl)))
      destination)))

(defun my/org-weekly-do-drafts (&optional today)
  "Create missing Do drafts for completed weeks with nonempty Roam dailies.
Copy whole daily notes into individual weekly Org files with DRAFT: true.
Skip the current Monday-Sunday week, future weeks, empty days, and existing
weekly files or visiting buffers.  Return new paths in chronological order.
When interactive, open the latest new draft.  TODAY is an optional date
string for calculating which weeks are complete; default to today."
  (interactive)
  (let* ((decoded (decode-time (if today (org-read-date nil t today) (current-time))))
         (current-monday
          (my/org-journal--monday
           (calendar-absolute-from-gregorian
            (list (nth 4 decoded) (nth 3 decoded) (nth 5 decoded)))))
         created)
    (dolist (week (my/org-journal--weeks current-monday))
      (let ((file (my/org-journal--write-week (car week) (cdr week))))
        (when file (push file created))))
    (setq created (nreverse created))
    (when (called-interactively-p 'interactive)
      (if created
          (progn
            (find-file (car (last created)))
            (message "Created %d weekly Do draft(s); opened the latest" (length created)))
        (message "No missing completed weeks with daily notes")))
    created))

(provide 'org-journal-weekly)
;;; org-journal-weekly.el ends here
