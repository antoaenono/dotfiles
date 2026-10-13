;;; org-glossary-export-fix.el --- Preserve only authored glossary breaks -*- lexical-binding: t; -*-

(require 'org-glossary)

(defun org-glossary-export-fix--trim-paragraph (value _backend _info)
  "Remove a single structural ending newline from inline glossary VALUE.
Work on a copied paragraph, preserving authored internal and explicit
line breaks.  Multi-paragraph and other block definitions are unchanged."
  (when (and (consp value)
             (null (cdr value))
             (eq (org-element-type (car value)) 'paragraph))
    (let* ((paragraph (org-element-copy (car value) t))
           (last-child (last (org-element-contents paragraph))))
      (when (and (stringp (car last-child))
                 (string-suffix-p "\n" (car last-child)))
        (setcar last-child (substring (car last-child) 0 -1))
        (list paragraph)))))

(defun org-glossary-export-fix--export-term (original entry info)
  "Export glossary ENTRY with ORIGINAL using an isolated copy of INFO.
Normalize only inline glossary paragraphs with preserved line breaks,
after existing parse-tree filters.  Keep the original export cache."
  (if (not (plist-get info :preserve-breaks))
      (funcall original entry info)
    (let ((export-info (copy-sequence info)))
      (setq export-info
            (plist-put
             export-info :filter-parse-tree
             (append
              (remq #'org-glossary-export-fix--trim-paragraph
                    (plist-get info :filter-parse-tree))
              (list #'org-glossary-export-fix--trim-paragraph))))
      (funcall original entry export-info))))

;; org-glossary exports a single paragraph's children inline.  With \n:t,
;; its structural ending newline otherwise becomes an HTML/LaTeX break.
(unless (advice-member-p #'org-glossary-export-fix--export-term
                        'org-glossary--export-term)
  (advice-add 'org-glossary--export-term :around
              #'org-glossary-export-fix--export-term))

(provide 'org-glossary-export-fix)
;;; org-glossary-export-fix.el ends here
