;;; tangle-moi.el --- Tangle the literate dotfiles config -*- lexical-binding: t; -*-

(require 'ob-tangle)
(require 'subr-x)

(defun tangle-moi--source-directory ()
  "Return the chezmoi source directory."
  (file-name-as-directory
   (string-trim (car (process-lines "chezmoi" "source-path")))))

(defun tangle-moi--config-file ()
  "Return the canonical config.org path."
  (file-truename
   (expand-file-name "config.org" (tangle-moi--source-directory))))

(defun tangle-moi-tangle ()
  "Tangle config.org while preserving any unsaved live Emacs buffer."
  (let* ((source-directory (tangle-moi--source-directory))
         (config-file (tangle-moi--config-file))
         (existing-buffer (find-buffer-visiting config-file)))
    (when (and existing-buffer (buffer-modified-p existing-buffer))
      (user-error "Refusing to tangle: config.org has unsaved Emacs changes"))
    (with-current-buffer (or existing-buffer (find-file-noselect config-file))
      (unless (verify-visited-file-modtime (current-buffer))
        (revert-buffer t t))
      (let ((default-directory source-directory))
        (save-restriction
          (widen)
          (org-babel-tangle))))))

(defun tangle-moi-tangle-batch ()
  "Tangle config.org and print the target files for batch callers."
  (condition-case error-data
      (progn
        (prin1 (tangle-moi-tangle))
        (terpri))
    (error
     (message "Tangle failed: %s" (error-message-string error-data))
     (kill-emacs 1))))

(defun tangle-moi--save-config-buffer ()
  "Save config.org when it has unsaved changes in Emacs."
  (when-let ((buffer (find-buffer-visiting (tangle-moi--config-file))))
    (when (buffer-modified-p buffer)
      (with-current-buffer buffer
        (save-buffer)))))

;;;###autoload
(defun tangle-moi ()
  "Tangle the literate dotfiles config, preview it, and ask to apply."
  (interactive)
  (tangle-moi--save-config-buffer)
  (message "Tangling Org file...")
  (tangle-moi-tangle)
  (message "Tangling complete. Previewing chezmoi changes...")
  (let ((diff (shell-command-to-string
               "chezmoi apply --dry-run --verbose --force")))
    (if (string-empty-p diff)
        (message "No changes to apply.")
      (with-output-to-temp-buffer "*chezmoi diff*"
        (princ diff))
      (if (y-or-n-p "Apply these changes? ")
          (if (zerop (shell-command "chezmoi apply --verbose"))
              (message "Changes applied.")
            (message "Chezmoi apply failed; see *Shell Command Output*."))
        (message "Changes aborted.")))))

(provide 'tangle-moi)
;;; tangle-moi.el ends here
