;;; org-journal-weekly-tests.el --- Journal workflow tests -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ert)
(require 'org-capture)
(load (expand-file-name "../lisp/org-journal-weekly.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(defmacro my/org-journal-test--with-files (journal &rest body)
  "Run BODY with a temporary JOURNAL and weekly output directory."
  (declare (indent 1))
  `(let* ((directory (make-temp-file "org-journal-test-" t))
          (my/org-journal-file (expand-file-name "journal.org" directory))
          (my/org-weekly-do-directory (expand-file-name "week/do/" directory))
          (org-capture-bookmark nil)
          (org-inhibit-startup t))
     (unwind-protect
         (progn
           (with-temp-file my/org-journal-file (insert ,journal))
           ,@body)
       (dolist (buffer (buffer-list))
         (when (and (buffer-file-name buffer)
                    (file-in-directory-p (buffer-file-name buffer) directory))
           (with-current-buffer buffer (set-buffer-modified-p nil))
           (kill-buffer buffer)))
       (delete-directory directory t))))

(defun my/org-journal-test--read (file)
  "Read FILE without visiting it."
  (with-temp-buffer (insert-file-contents file) (buffer-string)))

(ert-deftest my/org-weekly-do-default-never-prompts ()
  (my/org-journal-test--with-files "#+TITLE: Journal\n"
    (cl-letf (((symbol-function 'read-from-minibuffer)
               (lambda (&rest _) (ert-fail "Weekly catch-up prompted for input"))))
      (should-not (my/org-weekly-do-drafts)))
    (should-not (file-exists-p my/org-weekly-do-directory))))

(ert-deftest my/org-journal-capture-uses-one-date-tree ()
  (my/org-journal-test--with-files
      "#+TITLE: Journal\n* 2026\n** 2026-W43\n*** 2026-10-20 Tuesday\n"
    (let ((org-capture-templates (list (my/org-journal-capture-template))))
      (save-window-excursion
        (dolist (text '("First entry\nSomething I did" "Second entry\nA personal thought"))
          (let ((org-overriding-default-time (encode-time 0 30 13 20 10 2026)))
            (org-capture nil "j")
            (insert text)
            (org-capture-finalize)))))
    (let ((text (my/org-journal-test--read my/org-journal-file)))
      (should (string-match-p "^\\* 2026$" text))
      (should (string-match-p "^\\*\\* 2026-W43$" text))
      (should (= 1 (with-temp-buffer
                     (insert text)
                     (count-matches "^\\*\\*\\* 2026-10-20 " (point-min) (point-max)))))
      (should (string-match-p "^\\*\\*\\*\\* 13:30 First entry" text))
      (should (string-match-p "^\\*\\*\\*\\* 13:30 Second entry" text))
      (should-not (string-match-p "#\\+PUBLISH:" text)))))

(ert-deftest my/org-weekly-do-copies-whole-iso-week-as-draft ()
  (let ((journal
         (concat "#+TITLE: Journal\n* 2020\n** 2020-W52\n"
                 "*** 2020-12-27 Sunday\nPrevious week\n** 2020-W53\n"
                 ":PROPERTIES:\n:ID: original-week\n:END:\nWeek-level reflection\n"
                 "*** 2020-12-28 Monday\n:PROPERTIES:\n:ID: original-day\n:END:\n"
                 "Text before any Do heading\n**** 09:00 Walk\n:PROPERTIES:\n"
                 ":ID: original-entry\n:CUSTOM_ID: walk\n:END:\nPersonal thoughts\n"
                 "***** Details\n[[file:images/a\\[b\\].png][A photo]] and "
                 "[[file:notes.org::#topic][Notes]]\nfile:other.pdf\n"
                 "#+begin_example\n:ID: example-text\n[[file:untouched.pdf]]\n#+end_example\n"
                 "*** 2020-12-29 Tuesday\n\n*** 2021-01-03 Sunday\nLast day included\n"
                 "* 2021\n** 2021-W01\n*** 2021-01-04 Monday\nNext week\n")))
    (my/org-journal-test--with-files journal
      (let* ((drafts (my/org-weekly-do-drafts "2021-01-04"))
             (draft (car (last drafts)))
             (text (my/org-journal-test--read draft)))
        (should (equal (mapcar #'file-name-nondirectory drafts)
                       '("2020-W52.org" "2020-W53.org")))
        (should (equal (file-name-nondirectory draft) "2020-W53.org"))
        (dolist (pattern '("^#\\+TITLE: Week 53, 2020$"
                           "^#\\+DRAFT: true$" "^#\\+PUBLISH: time$"
                           "^#\\+DATE: <2020-12-28>$" "^\\* 2020-12-28 Monday$"
                           "^\\* 2021-01-03 Sunday$" "Text before any Do heading"
                           "Week-level reflection" "Personal thoughts" "^\\*\\*\\* Details$"
                           ":CUSTOM_ID: walk" ":ID: example-text"
                           "\\[\\[file:untouched.pdf\\]\\]"))
          (should (string-match-p pattern text)))
        (should-not (string-match-p "Previous week\\|Next week\\|:ID: original-\\|2020-12-29" text))
        (with-temp-buffer
          (insert text)
          (org-mode)
          (let ((links (org-element-map (org-element-parse-buffer) 'link #'identity)))
            (should (equal (org-element-property :path (nth 0 links))
                           (expand-file-name "images/a[b].png" directory)))
            (should (equal (org-element-property :path (nth 1 links))
                           (expand-file-name "notes.org" directory)))
            (should (equal (org-element-property :search-option (nth 1 links)) "#topic"))
            (should (equal (org-element-property :path (nth 2 links))
                           (expand-file-name "other.pdf" directory)))))
        (should (equal journal (my/org-journal-test--read my/org-journal-file)))
        (should-not (my/org-weekly-do-drafts "2021-01-04"))))))

(ert-deftest my/org-weekly-do-preserves-existing-draft ()
  (my/org-journal-test--with-files
      "* 2026\n** 2026-W43\n*** 2026-10-20 Tuesday\nThings I did\n"
    (let ((file (expand-file-name "2026-W43.org" my/org-weekly-do-directory)))
      (make-directory my/org-weekly-do-directory t)
      (with-temp-file file (insert "My edited weekly note\n#+DRAFT: false\n"))
      (should-not (my/org-weekly-do-drafts "2026-10-26"))
      (should (equal (my/org-journal-test--read file)
                     "My edited weekly note\n#+DRAFT: false\n")))))

(ert-deftest my/org-weekly-do-preserves-unsaved-draft ()
  (my/org-journal-test--with-files
      "* 2026\n** 2026-W43\n*** 2026-10-20 Tuesday\nThings I did\n"
    (make-directory my/org-weekly-do-directory t)
    (let* ((file (expand-file-name "2026-W43.org" my/org-weekly-do-directory))
           (buffer (find-file-noselect file)))
      (with-current-buffer buffer (insert "Work in progress"))
      (should-not (my/org-weekly-do-drafts "2026-10-26"))
      (should-not (file-exists-p file))
      (should (equal (with-current-buffer buffer (buffer-string)) "Work in progress")))))

(ert-deftest my/org-weekly-do-includes-unsaved-journal-without-changing-it ()
  (let ((journal "* 2026\n** 2026-W43\n*** 2026-10-20 Tuesday\nSaved text\n"))
    (my/org-journal-test--with-files journal
      (let ((buffer (find-file-noselect my/org-journal-file)))
        (with-current-buffer buffer
          (goto-char (point-max))
          (insert "Unsaved personal thought\n")
          (narrow-to-region (- (point-max) 10) (point-max))
          (let* ((minimum (point-min))
                 (position (point))
                 (draft (car (my/org-weekly-do-drafts "2026-10-26"))))
            (should (string-match-p "Unsaved personal thought"
                                    (my/org-journal-test--read draft)))
            (should (buffer-modified-p))
            (should (= (point-min) minimum))
            (should (= (point) position)))))
      (should (equal journal (my/org-journal-test--read my/org-journal-file))))))

(ert-deftest my/org-weekly-do-empty-week-creates-nothing ()
  (my/org-journal-test--with-files
      "* 2026\n** 2026-W42\n*** 2026-10-18 Sunday\n\n** 2026-W43\n*** 2026-10-19 Monday\n  \n"
    (should-not (my/org-weekly-do-drafts "2026-10-26"))
    (should-not (file-exists-p my/org-weekly-do-directory))))

(ert-deftest my/org-weekly-do-waits-until-the-following-monday ()
  (my/org-journal-test--with-files
      "* 2026\n** 2026-W43\n*** 2026-10-20 Tuesday\nTuesday notes\n*** 2026-10-25 Sunday\nSunday notes\n** 2026-W44\n*** 2026-10-26 Monday\nNext week's notes\n"
    (should-not (my/org-weekly-do-drafts "2026-10-25"))
    (should-not (file-exists-p my/org-weekly-do-directory))
    (let* ((drafts (my/org-weekly-do-drafts "2026-10-26"))
           (text (my/org-journal-test--read (car drafts))))
      (should (equal (mapcar #'file-name-nondirectory drafts) '("2026-W43.org")))
      (should (string-match-p "Tuesday notes" text))
      (should (string-match-p "Sunday notes" text))
      (should-not (string-match-p "Next week's notes" text)))))

;;; org-journal-weekly-tests.el ends here
