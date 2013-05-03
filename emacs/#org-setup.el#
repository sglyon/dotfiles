;; My org-mode setup
(setq org-directory "~/Dropbox/org/")
(setq org-default-notes-file (concat org-directory "capture.org"))

; Keybindings (from
; http://orgmode.org/worg/org-configs/org-config-examples.html)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key "\M-n" 'outline-next-visible-heading)
            (local-set-key "\M-p" 'outline-previous-visible-heading)
            ;; table
            (local-set-key "\M-\C-w" 'org-table-copy-region)
            (local-set-key "\M-\C-y" 'org-table-paste-rectangle)
            (local-set-key "\M-\C-l" 'org-table-sort-lines)
            ;; display images
            (local-set-key "\M-I" 'org-toggle-iimage-in-org)
            ;; fix tab
            (local-set-key "\C-y" 'yank)
            ;; yasnippet (allow yasnippet to do its thing in org files)
            (org-set-local 'yas/trigger-key [tab])
            (define-key yas/keymap [tab] 'yas/next-field-group)))

; Copied from online to get org mode setup
(require 'org-install)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((sh . true) (python . true))
)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; set up yasnippet
(require 'yasnippet)
(yas/initialize)
(setq yas/snippet-dirs '("~/.emacs.d/mysnippets"))
(mapc 'yas/load-directory yas/snippet-dirs)

;; set up org mode
(require 'org-install)
;; fix some org-mode + yasnippet conflicts:
(defun yas/org-very-safe-expand ()
  (let ((yas/fallback-behavior 'return-nil)) (yas/expand)))

(add-hook 'org-mode-hook
          (lambda ()
            (make-variable-buffer-local 'yas/trigger-key)
            (setq yas/trigger-key [tab])
            (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
            (define-key yas/keymap [tab] 'yas/next-field)))


;; ReFTeX setup
(defun org-mode-reftex-setup ()
  (load-library "reftex")
  (and (buffer-file-name) (file-exists-p (buffer-file-name))
       (progn
	 ;enable auto-revert-mode to update reftex when bibtex file changes on disk
	 (global-auto-revert-mode t)
	 (reftex-parse-all)
	 ;add a custom reftex cite format to insert links
	 (reftex-set-cite-format
	  '((?b . "[[bib:%l][%l-bib]]")
	    (?n . "[[notes:%l][%l-notes]]")
	    (?p . "[[papers:%l][%l-paper]]")
	    (?t . "%t")
	    (?h . "** %t\n:PROPERTIES:\n:Custom_ID: %l\n:END:\n[[papers:%l][%l-paper]]")))))
  (define-key org-mode-map (kbd "C-c )") 'reftex-citation)
  (define-key org-mode-map (kbd "C-c (") 'org-mode-reftex-search))

(add-hook 'org-mode-hook 'org-mode-reftex-setup)

