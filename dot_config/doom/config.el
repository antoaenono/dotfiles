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
  projectile-project-search-path '("~/source/" "~/org/" "~/.local/share/chezmoi/")
)

(defun tangle-moi ()
  "Tangle the current Org buffer, show the chezmoi diff, and ask to apply."
  (interactive)
  (save-buffer)
  (message "Tangling Org file...")
  (org-babel-tangle)
  (message "Tangling complete. Running 'chezmoi apply -vn'...")
  (let ((diff (shell-command-to-string "chezmoi apply -vn")))
    (if (string-empty-p diff)
        (message "No changes to apply.")
      (with-output-to-temp-buffer "*chezmoi diff*"
        (princ diff))
      (if (y-or-n-p "Apply these changes? ")
          (progn
            (message "Applying changes...")
            (shell-command "chezmoi apply -v")
            (message "Changes applied."))
        (message "Changes aborted.")))))


;; 80 char limit encourages concise code. Respect to the early days.
;; note: in org files 80 chars is /on top/ of the spacing under the heading
(setq-default fill-column 80)
(global-display-fill-column-indicator-mode 1)
 
;; Rebind "quit" to Cmd-Shift-q to avoid conflict with M-q (fill-paragraph)
;; ...shouldn't be so easy to quit anyway!
(map! :g "M-Q" #'save-buffers-kill-terminal)
;; TODO the above doesn't work, it just adds a new binding, and Emacs Gui still captures Cmd+q

(global-tab-line-mode 1) ; i see buffers in windows
(tab-bar-mode 1) ; i see windows in frames

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

;; ORG ;;
(setopt org-directory "~/org/")

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

;; lorem epsom
;; default is gemini, key in authinfo.gpg
(use-package! gptel
  :config
  (setopt
   gptel-log-level 'debug ;; see *gptel-log* buffer
   gptel-include-reasoning t
   gptel-include-tool-results t
   gptel-track-response t
  )
  ;; org mode for interacting with LLMs?!
  (setopt
   gptel-default-mode 'org-mode
   gptel-org-branching-context t
  )
  ;; if branching-context, prevent default prompt prefix from further nesting headings
  (setf (alist-get 'org-mode gptel-prompt-prefix-alist) "@model\n")
  (setf (alist-get 'org-mode gptel-response-prefix-alist) "@model\n")
  ;; Register the Ollama backend so it's available from the menu
  (gptel-make-ollama "Ollama"
    :host "localhost:11434"
    :stream t
    :models '("gpt-oss" "qwen3"))
  ;; Register Gemini and set it as the default backend.
  ;; TODO gptel should automatically fetch the key from your .authinfo.gpg file, but i kept getting nil
  (setopt gptel-backend (gptel-make-gemini "Gemini" :stream t :key (gptel-api-key-from-auth-source "generativelanguage.googleapis.com")))
  ;; (Optional) Set a default model for the Gemini backend
  (setopt gptel-model 'gemini-pro-latest))


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
