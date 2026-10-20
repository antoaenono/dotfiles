;;; org-journal-weekly.el --- Private journal to weekly drafts -*- lexical-binding: t; -*-

(require 'org)
(require 'calendar)
(require 'cal-iso)

(defvar my/org-journal-file "~/org/journal.org"
  "Private, single-file journal with a year/week/day date tree.")

(defvar my/org-weekly-do-directory "~/org/time/week/do/"
  "Directory for editable weekly Do drafts.")

(defun my/org-journal-capture-template ()
  "Return the native date-tree capture template for the private journal."
  `("j" "Journal" entry (file+olp+datetree ,my/org-journal-file)
    "* %<%H:%M> %?\n" :tree-type week :empty-lines 1))

(defun my/org-journal--week-monday (heading)
  "Return the Monday of a native ISO week HEADING as an absolute date."
  (let ((title (org-element-property :raw-value heading)))
    (when (and (= (org-element-property :level heading) 2)
               (string-match
                "\\`\\([0-9]\\{4\\}\\)-W\\([0-9]\\{2\\}\\)\\'"
                title))
      (let* ((iso (list (string-to-number (match-string 2 title)) 1
                        (string-to-number (match-string 1 title))))
             (monday (calendar-iso-to-absolute iso)))
        (when (equal iso (calendar-iso-from-absolute monday)) monday)))))

(defun my/org-journal--weeks (current-monday)
  "Return completed week subtrees paired with their absolute Monday dates.
Include unsaved journal edits without modifying the source buffer, its
narrowing, or the file on disk.  Exclude CURRENT-MONDAY and later weeks."
  (let* ((journal (expand-file-name my/org-journal-file))
         (source-buffer (find-buffer-visiting journal))
         entries)
    (with-temp-buffer
      (if source-buffer
          (insert (with-current-buffer source-buffer
                    (save-restriction
                      (widen)
                      (buffer-substring-no-properties (point-min) (point-max)))))
        (insert-file-contents journal))
      (delay-mode-hooks (org-mode))
      (org-element-map (org-element-parse-buffer) 'headline
        (lambda (heading)
          (let ((monday (my/org-journal--week-monday heading)))
            (when (and monday (< monday current-monday))
              (push (cons monday (buffer-substring-no-properties
                                  (org-element-property :begin heading)
                                  (org-element-property :end heading)))
                    entries))))))
    (sort entries (lambda (a b) (< (car a) (car b))))))

(defun my/org-journal--weekly-body (text)
  "Copy week subtree TEXT for a draft, preserving its local file links.
Drop the week heading and empty days, promote days to level one, and remove
copied global IDs.  Keep the journal's IDs and copied file-local CUSTOM_IDs."
  (with-temp-buffer
    (insert text)
    (delay-mode-hooks (org-mode))
    (let ((source-directory (file-name-directory
                             (expand-file-name my/org-journal-file)))
          (org-odd-levels-only nil)
          (org-adapt-indentation nil))
      (org-map-entries (lambda () (org-entry-delete nil "ID")))
      (dolist (heading (reverse (org-element-map (org-element-parse-buffer)
                                   'headline #'identity)))
        (when (and (= (org-element-property :level heading) 3)
                   (not (org-element-contents heading)))
          (delete-region (org-element-property :begin heading)
                         (org-element-property :end heading))))
      (goto-char (point-min))
      (delete-region (point-min) (line-beginning-position 2))
      (org-map-entries (lambda () (dotimes (_ 2) (org-promote))))
      ;; Parse links so examples and code blocks are left alone.  Edit from
      ;; the end to keep earlier element positions valid.
      (dolist (link (reverse (org-element-map (org-element-parse-buffer)
                                'link #'identity)))
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
                                   ":" (expand-file-name path source-directory)
                                   (and search (concat "::" search)))))
              (delete-region begin end)
              (goto-char begin)
              (insert (org-link-make-string target description)))))))
    (buffer-string)))

(defun my/org-journal--monday (day)
  "Return the Monday of the ISO week containing absolute calendar DAY."
  ;; Calendar represents Sunday as zero, so wrap it back to Monday.
  (- day (mod (1- (nth 1 (calendar-iso-from-absolute day))) 7)))

(defun my/org-journal--write-week (monday text)
  "Write a missing weekly draft for MONDAY from week subtree TEXT.
Return its path when created, otherwise nil.  Never replace an existing
file or visiting buffer.  Empty weeks produce no file."
  (let* ((iso (calendar-iso-from-absolute monday))
         (week (format "%d-W%02d" (nth 2 iso) (car iso)))
         (destination (expand-file-name (concat week ".org")
                                        my/org-weekly-do-directory)))
    (unless (or (file-exists-p destination) (find-buffer-visiting destination))
      (let ((start (calendar-gregorian-from-absolute monday))
            (body (my/org-journal--weekly-body text)))
        (when (string-match-p "\\S-" body)
          (with-temp-buffer
            (insert (format "#+TITLE: Week %d, %d\n#+DATE: <%04d-%02d-%02d>\n#+PUBLISH: time\n#+DRAFT: true\n\n"
                            (car iso) (nth 2 iso) (nth 2 start) (car start) (nth 1 start))
                    body)
            (unless (bolp) (insert "\n"))
            (make-directory (file-name-directory destination) t)
            ;; Refuse to overwrite even if another process creates it meanwhile.
            (write-region (point-min) (point-max) destination nil 'silent nil 'excl))
          destination)))))

(defun my/org-weekly-do-drafts (&optional today)
  "Create missing Do drafts for every completed week with journal entries.
Copy each whole week subtree into an individual Org file with DRAFT: true.  Skip
empty days, the current Monday-Sunday week, future weeks, and all existing
weekly files or visiting buffers.  Return newly created paths in date order.
When interactive, open the most recent new draft.  TODAY is an optional
date string for calculating which weeks are complete; default to today."
  (interactive)
  (let* ((decoded (decode-time (if today (org-read-date nil t today) (current-time))))
         (current-monday
          (my/org-journal--monday
           (calendar-absolute-from-gregorian
            (list (nth 4 decoded) (nth 3 decoded) (nth 5 decoded)))))
         (weeks (my/org-journal--weeks current-monday))
         created)
    (dolist (week weeks)
      (let ((file (my/org-journal--write-week (car week) (cdr week))))
        (when file (push file created))))
    (setq created (nreverse created))
    (when (called-interactively-p 'interactive)
      (if created
          (progn
            (find-file (car (last created)))
            (message "Created %d weekly Do draft(s); opened the latest" (length created)))
        (message "No missing completed weeks with journal entries")))
    created))

(provide 'org-journal-weekly)
;;; org-journal-weekly.el ends here
