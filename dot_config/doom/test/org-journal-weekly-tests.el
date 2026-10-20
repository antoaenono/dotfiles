;;; org-journal-weekly-tests.el --- Roam daily weekly drafts -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ert)
(load (expand-file-name "../lisp/org-journal-weekly.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(defmacro my/org-journal-test--with-files (&rest body)
  "Run BODY with temporary native Roam dailies and weekly output directories."
  (declare (indent 0))
  `(let* ((directory (make-temp-file "org-dailies-test-" t))
          (org-roam-directory (expand-file-name "roam/" directory))
          (org-roam-dailies-directory "daily/")
          (my/org-weekly-do-directory (expand-file-name "week/do/" directory))
          (org-inhibit-startup t))
     (unwind-protect
         (progn
           (make-directory (expand-file-name org-roam-dailies-directory org-roam-directory) t)
           ,@body)
       (dolist (buffer (buffer-list))
         (when (and (buffer-file-name buffer)
                    (file-in-directory-p (buffer-file-name buffer) directory))
           (with-current-buffer buffer (set-buffer-modified-p nil))
           (kill-buffer buffer)))
       (delete-directory directory t))))

(defun my/org-journal-test--daily (date body)
  "Write a daily note with native file metadata for DATE and BODY."
  (let ((file (expand-file-name (concat date ".org")
                                (expand-file-name org-roam-dailies-directory
                                                  org-roam-directory))))
    (with-temp-file file
      (insert (format ":PROPERTIES:\n:ID: file-%s\n:END:\n#+title: %s\n\n%s" date date body)))
    file))

(defun my/org-journal-test--read (file)
  "Read FILE without visiting it."
  (with-temp-buffer (insert-file-contents file) (buffer-string)))

(ert-deftest my/org-weekly-do-default-never-prompts ()
  (my/org-journal-test--with-files
    (my/org-journal-test--daily "README" "Daily folder instructions\n")
    (cl-letf (((symbol-function 'read-from-minibuffer)
               (lambda (&rest _) (ert-fail "Weekly catch-up prompted for input"))))
      (should-not (my/org-weekly-do-drafts)))
    (should-not (file-exists-p my/org-weekly-do-directory))))

(ert-deftest my/org-weekly-do-copies-whole-iso-week-as-draft ()
  (my/org-journal-test--with-files
    (my/org-journal-test--daily "2020-12-27" "Previous week\n")
    (let* ((file
            (my/org-journal-test--daily
             "2020-12-28"
             (concat "#+PUBLISH: til\n#+DRAFT: false\nText before any Do heading\n"
                     "* 09:00 Walk\n:PROPERTIES:\n:ID: original-entry\n:CUSTOM_ID: walk\n:END:\n"
                     "Personal thoughts\n** Details\n[[file:images/a\\[b\\].png][A photo]] and "
                     "[[file:notes.org::#topic][Notes]]\nfile:other.pdf\n"
                     "#+begin_example\n:ID: example-text\n[[file:untouched.pdf]]\n#+end_example\n")))
           (original (my/org-journal-test--read file)))
      (my/org-journal-test--daily "2020-12-29" "  \n")
      (my/org-journal-test--daily "2021-01-03" "Last day included\n")
      (my/org-journal-test--daily "2021-01-04" "Next week\n")
      (let* ((drafts (my/org-weekly-do-drafts "2021-01-04"))
             (text (my/org-journal-test--read (car (last drafts)))))
        (should (equal (mapcar #'file-name-nondirectory drafts)
                       '("2020-W52.org" "2020-W53.org")))
        (dolist (pattern '("^#\\+TITLE: Week 53, 2020$" "^#\\+DRAFT: true$"
                           "^#\\+PUBLISH: time$" "^#\\+DATE: <2020-12-28>$"
                           "^\\* 2020-12-28 Monday$" "^\\* 2021-01-03 Sunday$"
                           "Text before any Do heading" "Personal thoughts"
                           "^\\*\\*\\* Details$" ":CUSTOM_ID: walk" ":ID: example-text"
                           "\\[\\[file:untouched.pdf\\]\\]"))
          (should (string-match-p pattern text)))
        (should-not (string-match-p
                     "Previous week\\|Next week\\|2020-12-29\\|:ID: file-\\|:ID: original-\\|#\\+PUBLISH: til\\|#\\+DRAFT: false" text))
        (with-temp-buffer
          (insert text)
          (org-mode)
          (let* ((links (org-element-map (org-element-parse-buffer) 'link #'identity))
                 (daily-directory (file-name-directory file)))
            (should (equal (org-element-property :path (nth 0 links))
                           (expand-file-name "images/a[b].png" daily-directory)))
            (should (equal (org-element-property :path (nth 1 links))
                           (expand-file-name "notes.org" daily-directory)))
            (should (equal (org-element-property :search-option (nth 1 links)) "#topic"))
            (should (equal (org-element-property :path (nth 2 links))
                           (expand-file-name "other.pdf" daily-directory)))))
        (should (equal original (my/org-journal-test--read file)))
        (should-not (my/org-weekly-do-drafts "2021-01-04"))))))

(ert-deftest my/org-weekly-do-preserves-existing-draft ()
  (my/org-journal-test--with-files
    (my/org-journal-test--daily "2026-10-20" "Things I did\n")
    (let ((file (expand-file-name "2026-W43.org" my/org-weekly-do-directory)))
      (make-directory my/org-weekly-do-directory t)
      (with-temp-file file (insert "My edited weekly note\n#+DRAFT: false\n"))
      (should-not (my/org-weekly-do-drafts "2026-10-26"))
      (should (equal (my/org-journal-test--read file)
                     "My edited weekly note\n#+DRAFT: false\n")))))

(ert-deftest my/org-weekly-do-preserves-unsaved-draft ()
  (my/org-journal-test--with-files
    (my/org-journal-test--daily "2026-10-20" "Things I did\n")
    (make-directory my/org-weekly-do-directory t)
    (let* ((file (expand-file-name "2026-W43.org" my/org-weekly-do-directory))
           (buffer (find-file-noselect file)))
      (with-current-buffer buffer (insert "Work in progress"))
      (should-not (my/org-weekly-do-drafts "2026-10-26"))
      (should-not (file-exists-p file))
      (should (equal (with-current-buffer buffer (buffer-string)) "Work in progress")))))

(ert-deftest my/org-weekly-do-includes-unsaved-daily-without-changing-it ()
  (my/org-journal-test--with-files
    (let* ((file (my/org-journal-test--daily "2026-10-20" "Saved text\n"))
           (original (my/org-journal-test--read file))
           (buffer (find-file-noselect file)))
      (with-current-buffer buffer
        (goto-char (point-max))
        (insert "Unsaved personal thought\n")
        (narrow-to-region (- (point-max) 10) (point-max))
        (let* ((minimum (point-min))
               (position (point))
               (draft (car (my/org-weekly-do-drafts "2026-10-26"))))
          (should (string-match-p "Unsaved personal thought" (my/org-journal-test--read draft)))
          (should (buffer-modified-p))
          (should (= (point-min) minimum))
          (should (= (point) position))))
      (should (equal original (my/org-journal-test--read file))))))

(ert-deftest my/org-weekly-do-empty-week-creates-nothing ()
  (my/org-journal-test--with-files
    (my/org-journal-test--daily "2026-10-18" "\n")
    (my/org-journal-test--daily "2026-10-19" "* \n")
    (should-not (my/org-weekly-do-drafts "2026-10-26"))
    (should-not (file-exists-p my/org-weekly-do-directory))))

(ert-deftest my/org-weekly-do-waits-until-the-following-monday ()
  (my/org-journal-test--with-files
    (my/org-journal-test--daily "2026-10-20" "Tuesday notes\n")
    (my/org-journal-test--daily "2026-10-25" "Sunday notes\n")
    (my/org-journal-test--daily "2026-10-26" "Next week's notes\n")
    (should-not (my/org-weekly-do-drafts "2026-10-25"))
    (should-not (file-exists-p my/org-weekly-do-directory))
    (let* ((drafts (my/org-weekly-do-drafts "2026-10-26"))
           (text (my/org-journal-test--read (car drafts))))
      (should (equal (mapcar #'file-name-nondirectory drafts) '("2026-W43.org")))
      (should (string-match-p "Tuesday notes" text))
      (should (string-match-p "Sunday notes" text))
      (should-not (string-match-p "Next week's notes" text)))))

(ert-deftest my/org-weekly-do-catches-up-older-weeks-without-replacing-newer ()
  (my/org-journal-test--with-files
    (my/org-journal-test--daily "2026-10-05" "Older week\n")
    (my/org-journal-test--daily "2026-10-12" "Newer week\n")
    (make-directory my/org-weekly-do-directory t)
    (let ((existing (expand-file-name "2026-W42.org" my/org-weekly-do-directory)))
      (with-temp-file existing (insert "Already edited\n"))
      (should (equal (mapcar #'file-name-nondirectory (my/org-weekly-do-drafts "2026-10-20"))
                     '("2026-W41.org")))
      (should (equal (my/org-journal-test--read existing) "Already edited\n")))))

;;; org-journal-weekly-tests.el ends here
