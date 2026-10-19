;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

; Remember, you do not need to run 'doom sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

(setopt
  user-full-name "Anton Bilbaeno"
  user-mail-address "antoaenono@pm.me"
  display-line-numbers-type 'relative
  mouse-drag-copy-region t
)

(setq
  doom-theme 'doom-monokai-machine
)

(setq
  projectile-project-search-path '("~/source/" "~/.local/share/chezmoi/")
  projectile-globally-ignored-directories '("node_modules" ".cache" "deps" "_build" "dist" ".elixir_ls" ".hex" ".mix")
  projectile-ignored-projects '("~/source/archive/")
)

(load! "lisp/tangle-moi")


;; 80 char limit encourages concise code. Respect to the early days.
;; note: in org files 80 chars is /on top/ of the spacing under the heading
(setq-default fill-column 80)
(global-display-fill-column-indicator-mode 1)

;; Rebind "quit" to Cmd-Shift-q to avoid conflict with M-q (fill-paragraph)
;; ...shouldn't be so easy to quit anyway!
(map! :g "M-Q" #'save-buffers-kill-terminal)
;; TODO the above doesn't work, it just adds a new binding, and Emacs Gui still captures Cmd+q


;; HASKELL
;; Hooks so haskell and literate haskell major modes trigger LSP setup
(add-hook 'haskell-mode-hook #'lsp)
(add-hook 'haskell-literate-mode-hook #'lsp)

;; TODO test if these are really needed after moving path to .zshenv (was in .zshrc)
;; Add haskell lsp to path for emacs subprocesses
(add-to-list 'exec-path (expand-file-name "~/.ghcup/bin"))
;; Add haskell tools to path for emacs environment
(setenv "PATH" (concat (expand-file-name "~/.ghcup/bin") ":" (getenv "PATH")))
;; hlint in local bin. Ensure ~/.local/bin is in Emacs's exec-path
;; TODO this didnt work emacs still can't find hlint
(setq exec-path (cons (expand-file-name "~/.local/bin") exec-path))

;; Counted normal-state j/k match relative buffer-line numbers across folds.
;; Uncounted motions and operator/visual-state bindings retain Evil's defaults.
(defcustom my/counted-motions-reveal nil
  "Whether counted j/k reveal a destination inside folded text."
  :type 'boolean
  :group 'evil)

(after! evil
  (defun my/move-buffer-lines (count)
    "Move COUNT buffer lines, resolving hidden destinations without guessing."
    (let ((line-move-visual nil)
          (line-move-ignore-invisible nil))
      (evil-line-move count))
    (when (invisible-p (point))
      (if my/counted-motions-reveal
          (cond ((derived-mode-p 'org-mode) (org-fold-reveal))
                ((derived-mode-p 'markdown-mode) (markdown-show-entry))
                (t (+fold/open)))
        ;; Leave folds closed and stop at the boundary in the motion direction.
        (while (and (invisible-p (point))
                    (if (> count 0) (not (eobp)) (not (bobp))))
          (goto-char
           (if (> count 0)
               (next-char-property-change (point) (point-max))
             (max (point-min)
                  (1- (previous-char-property-change (point) (point-min)))))))
        ;; Org can leave a visible newline at the end of a hidden span.
        (when (and (> count 0) (eolp) (not (eobp)))
          (forward-char 1))))
    (evil-adjust-cursor))

  (evil-define-motion my/next-line (count)
    "Move by buffer lines with COUNT and relative numbers, otherwise normally."
    :type line
    (if (and count (eq display-line-numbers 'relative))
        (my/move-buffer-lines count)
      (evil-next-line count)))

  (evil-define-motion my/previous-line (count)
    "Move up by buffer lines with COUNT and relative numbers, otherwise normally."
    :type line
    (if (and count (eq display-line-numbers 'relative))
        (my/move-buffer-lines (- count))
      (evil-previous-line count)))

  (define-key evil-normal-state-map "j" #'my/next-line)
  (define-key evil-normal-state-map "k" #'my/previous-line))

;; ORG ;;
(setopt org-directory "~/org/")

;; Restore Org's native folded -> children -> subtree TAB cycle. Doom's
;; evil-org integration otherwise replaces it with a two-state local toggle.
(after! evil-org
  (remove-hook 'org-tab-first-hook #'+org-cycle-only-current-subtree-h))

;; Disable org-element cache (Org 9.5+ bug): subtree and narrowed exports trigger
;; cache revalidation on the temp export buffer, causing an infinite loop / hang.
;; Full-document export is unaffected. Disabling the cache trades minor parse speed
;; for reliable export at any scope.
(setq org-element-use-cache nil)

;; (after! org
;;   ;; Add the 'titletoc' package to Org's list of LaTeX packages
;;   (add-to-list 'org-latex-packages-alist '("titletoc" nil t))

;;   ;; Use latexmk for PDF export
;;   ;; (setq org-latex-to-pdf-process (list "latexmk %f"))
;; )

;; more `<` tab completion templates aka "structure templates" -- orgmode.org/manual/Structure-Templates.html
;(after! org (add-to-list 'org-structure-template-alist
  ;("sh" . "src shell")))

;; custom syntax: !! to indicate key presses
;; add as a keyword in org-mode and draw a box around it
;; TODO export to <kbd>
(after! org
  (font-lock-add-keywords
   'org-mode
   '(("!!\\(.*?\\)!!"
      (1 '(face (:weight 'semi-bold
                           :box '(:line-width 2 :color "palegoldenrod"))))))))

;; automatically show magic behind point
(use-package! org-appear
  :after org
  :hook (org-mode . org-appear-mode)
  :config
  (setopt org-appear-autoemphasis t ; _/**/_
          org-appear-autolinks t ;; dun work on #+TRANSCLUSION directives. use `org-toggle-link-display`
          org-appear-autoentities t ;; \alpha for α
  )
)

; information teleport
(use-package! org-transclusion
  :after org
  :init
  (map!
   :map global-map "<f12>" #'org-transclusion-add
   :leader
   :prefix "n"
   :desc "Org Transclusion Mode" "t" #'org-transclusion-mode))

; https://nobiot.github.io/org-transclusion/#:~:text=4.10%20Extensions,regain%20the%20indentation.
; this didn't seem to work, or i don't understand what it's supposed to be doing
; but i was trying to fix the issue where the source buffer loses indentation while transclusion is active
  ; :config
  ; ;; Enable org-transclusion-indent-mode extension
  ; (add-to-list 'org-transclusion-extensions 'org-transclusion-indent-mode)
  ; (require 'org-transclusion-indent-mode))

; roaman emacs... to gather
(setopt org-roam-directory "~/org/roam")
(org-roam-db-autosync-mode) ; docs say put this here[?]

;; CITATIONS (citar + org-cite)
(setq! citar-bibliography '("~/org/bib/references.bib")
       citar-library-paths '("~/org/sources/")
       citar-library-paths-recursive t
       citar-notes-paths '("~/org/roam/refs/"))

(after! oc
  (setq org-cite-global-bibliography '("~/org/bib/references.bib")
        org-cite-insert-processor 'citar
        org-cite-follow-processor 'citar
        org-cite-activate-processor 'citar))

(defun my/citar-noter-document (citekey)
  "Resolve one attachment for CITEKEY, preferring PDF for Org Noter."
  (let* ((files (delete-dups (gethash citekey (citar-get-files citekey))))
         (document (or (seq-find (lambda (file)
                                   (string-equal (downcase (or (file-name-extension file) ""))
                                                 "pdf"))
                                 files)
                       (car files))))
    (if document (abbreviate-file-name document) "")))

;; bridge citar with org-roam for note management
(use-package! citar-org-roam
  :after (citar org-roam)
  :config
  (citar-org-roam-mode)
  (setq citar-org-roam-subdir "refs"
        citar-org-roam-note-title-template "${author}, ${title}"
        citar-org-roam-template-fields
        '((:citar-title   . ("title"))
          (:citar-author  . ("author" "editor"))
          (:citar-date    . ("date" "year" "issued"))
          (:citar-pages   . ("pages"))
          (:citar-type    . ("=type="))
          (:citar-file    . ("file"))))
  (add-to-list 'org-roam-capture-templates
               '("r" "reference" plain "%?"
                 :if-new (file+head "refs/${citar-citekey}.org"
                                    ":PROPERTIES:\n:NOTER_DOCUMENT: %(my/citar-noter-document \"${citar-citekey}\")\n:END:\n#+title: ${citar-title}\n#+DATE: %<%Y-%m-%d %a>\n#+PUBLISH: garden\n#+DRAFT: true\n")
                 :immediate-finish t
                 :unnarrowed t))
  (setq citar-org-roam-capture-template-key "r"))

(defun my/citar-open-noter ()
  "Open roam note and start org-noter in a dedicated frame."
  (interactive)
  (select-frame (make-frame))
  (call-interactively #'citar-open-notes)
  (org-noter))

(map! :leader :desc "Open noter session" "n p" #'my/citar-open-noter)

(defun my/citar-close-noter ()
  "Save notes and PDF annotations, end org-noter session, and close frame."
  (interactive)
  (org-noter--with-valid-session
    (with-current-buffer (org-noter--session-notes-buffer session)
      (save-buffer))
    (with-current-buffer (org-noter--session-doc-buffer session)
      (save-buffer)))
  (org-noter-kill-session)
  (delete-frame))

(map! :leader :desc "Close noter session" "n q" #'my/citar-close-noter)

;; PDF ANNOTATION (org-noter)
(use-package! org-noter
  :after org
  :config
  (setq org-noter-always-create-frame nil
        org-noter-kill-frame-at-session-end nil
        org-noter-auto-save-last-location nil
        org-noter-doc-property-in-notes t
        org-noter-notes-window-location 'horizontal-split
        org-noter-highlight-selected-text t))

;; GLOSSARY / ACRONYMS
(setq org-link-descriptive t
      org-glossary-global-terms '("~/org/glossary.org"))
(use-package! org-glossary
  :after org
  :hook (org-mode . org-glossary-mode)
  :config
  (load! "lisp/org-glossary-export-fix"))

;; PEOPLE REFERENCES
(use-package! org-contacts
  :after org
  :config
  (setq org-contacts-files '("~/org/contacts.org")
        org-contacts-matcher "contact"
        org-contacts-tag "contact")
  (add-to-list 'org-tags-exclude-from-inheritance "contact")

  (defun my/org-contact-description (link _description)
    "Default LINK's local description to its nickname or initials."
    (let* ((name (org-link-unescape
                  (replace-regexp-in-string "\\`\\(?:org-contact\\|who\\):" "" link)))
           (contact (car (org-contacts-filter
                          (concat "\\`" (regexp-quote name) "\\'"))))
           (nickname (cdr (assoc-string "NICKNAME" (nth 2 contact)))))
      (if (and nickname (not (string-empty-p (string-trim nickname))))
          (string-trim nickname)
        (mapconcat (lambda (word) (upcase (substring word 0 1)))
                   (split-string name "[[:space:]-]+" t) ""))))

  (org-link-set-parameters "org-contact"
                           :insert-description #'my/org-contact-description)

  (defun my/org-contact-short-complete (&optional arg)
    "Complete a who: link using org-contacts."
    (concat "who:" (string-remove-prefix "org-contact:"
                                         (org-contacts-link-complete arg))))

  (org-link-set-parameters "who"
                           :follow #'org-contacts-link-open
                           :complete #'my/org-contact-short-complete
                           :insert-description #'my/org-contact-description)

  (require 'url-util)

  (defun my/org-contact-social-url (property value)
    "Resolve a GitHub or LinkedIn PROPERTY VALUE to its profile URL."
    (when-let ((template (cdr (assoc-string
                              property
                              '(("GITHUB" . "https://github.com/%s")
                                ("LINKEDIN" . "https://www.linkedin.com/in/%s/"))
                              t))))
      (let* ((value (string-trim value))
             (handle (string-remove-prefix "@" value)))
        (cond ((string-match-p "\\`https?://[^[:space:]]+\\'" value) value)
              ((string-match-p "\\`[[:alnum:]][[:alnum:]._-]*\\'" handle)
               (format template (url-hexify-string handle)))))))

  (defun my/org-open-contact-social-property (&rest _)
    "Open a GitHub or LinkedIn profile from the Org property at point."
    (when (derived-mode-p 'org-mode)
      (let ((property (org-element-context)))
        (when (eq (org-element-type property) 'node-property)
          (when-let ((url (my/org-contact-social-url
                          (org-element-property :key property)
                          (org-element-property :value property))))
            (browse-url url)
            t)))))

  (add-hook 'org-open-at-point-functions #'my/org-open-contact-social-property)
  (advice-add '+org/dwim-at-point :before-until #'my/org-open-contact-social-property))

(defun my/org-insert-contact-link ()
  "Select a contact using org-contacts and insert its default label."
  (interactive)
  (require 'org-contacts)
  (org-insert-link nil (my/org-contact-short-complete)))

(map! :leader :desc "Insert contact reference" "n P" #'my/org-insert-contact-link)

;; ORG HEADING FACES (levels 5-8 inherit outline-4/foreground by default)
(custom-set-faces!
  '(org-level-5 :foreground "#f92672")  ; pink
  '(org-level-6 :foreground "#fd971f")  ; orange
  '(org-level-7 :foreground "#ae81ff")  ; purple
  '(org-level-8 :foreground "#66d9ef")) ; teal

;; ROAM UI (graph visualization)
(use-package! org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t))

;; CAPTURE TEMPLATES
(after! org
  (setq org-capture-templates
        '(("t" "Task" entry (file+headline "~/org/inbox.org" "Tasks")
           "* TODO %?\n%U\n%a\n")
          ("n" "Note" entry (file+headline "~/org/inbox.org" "Notes")
           "* %?\n%U\n")
          ("r" "Reading Note" entry (file+headline "~/org/inbox.org" "Reading")
           "* %^{Title}\n%U\n%?\n")
          ("p" "Person" entry (file "~/org/contacts.org")
           "* %^{Name} :contact:\n:PROPERTIES:\n:ID: %(org-id-new)\n:NICKNAME: %^{Nickname}\n:PRIVATE: true\n:END:\n%?\n"))))

;; REFILE
(after! org
  (setq org-refile-targets '((nil :maxlevel . 3)
                             (org-agenda-files :maxlevel . 3))
        org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil
        org-refile-allow-creating-parent-nodes 'confirm))

;; AGENDA (recursive - finds TODOs in all subdirs)
(setq org-agenda-files '("~/org/")
      org-agenda-file-regexp "\\`[^.].*\\.org\\'")
(setq org-agenda-custom-commands
      '(("d" "Dashboard"
         ((agenda "" ((org-agenda-span 14)))
          (todo "TODO" ((org-agenda-overriding-header "Tasks")))))))

(defun my/org-agenda-files-recursive ()
  "Build agenda file list recursively from ~/org/."
  (directory-files-recursively "~/org/" "\\.org$"))
(setq org-agenda-files (my/org-agenda-files-recursive))

(defun my/insert-site-file-header (publish-type)
  "Insert a dated draft site header routed to PUBLISH-TYPE."
  (let ((title
         (capitalize
          (string-replace
           "-" " "
           (file-name-base buffer-file-name)))))
    (insert
     (format "#+title: %s\n#+DATE: %s\n#+PUBLISH: %s\n#+DRAFT: true\n\n"
             title
             (format-time-string "<%Y-%m-%d %a>")
             publish-type))))

;; Publishing is metadata-driven, so source location and site destination can
;; vary independently. These rules provide the common directory defaults.
(set-file-template!
 (concat "\\`"
         (regexp-quote (expand-file-name "~/org/writing/"))
         ".+\\.org\\'")
 :trigger (lambda () (my/insert-site-file-header "writing")))

(set-file-template!
 (concat "\\`"
         (regexp-quote (expand-file-name "~/org/works/"))
         ".+\\.org\\'")
 :trigger (lambda () (my/insert-site-file-header "works")))

(set-file-template!
 (concat "\\`"
         (regexp-quote (expand-file-name "~/org/now/"))
         "[^/]+\\.org\\'")
 :trigger (lambda () (my/insert-site-file-header "now")))

(set-file-template!
 (concat "\\`"
         (regexp-quote (expand-file-name "~/org/time/"))
         ".+\\.org\\'")
 :trigger (lambda () (my/insert-site-file-header "time")))

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;;; magit
(after! magit
  (setq magit-diff-refine-hunk 'all
        magit-diff-highlight-trailing nil
        magit-diff-paint-whitespace nil))

(custom-set-faces!
  '(magit-diff-added             :background unspecified)
  '(magit-diff-added-highlight   :background unspecified)
  '(magit-diff-removed           :background unspecified)
  '(magit-diff-removed-highlight :background unspecified))

;;; difftastic - AST-aware diffs, called on demand from magit
(use-package! difftastic
  :defer t
  :init
  (map! :leader
        :prefix "g"
        :desc "Difftastic show commit"   "D" #'difftastic-magit-show
        :desc "Difftastic diff range"    "T" #'difftastic-magit-diff)
  ;; Note: difftastic lands in doom's bottom popup (small). Press `C-w _` to
  ;; maximize the window vertically, or `C-w o` to fill the frame.
  )
