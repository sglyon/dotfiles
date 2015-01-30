; Tell emacs about theme directory for colorschemes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

; Set defualt directory
(let ((default-directory "~/.emacs.d"))
  (normal-top-level-add-subdirs-to-load-path))


 ;;----------------- General Settings
(setq user-mail-address "spencerlyon2@gmail.com")
(setq user-full-name "Spencer Lyon")

; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; New shutdown function that calls org-mobile-push automatically
(defun intelligent-close ()
  (interactive)
  (org-mobile-push) ;; <== MOBILE ==
  (if (eq (car (visible-frame-list)) (selected-frame))
      (if (> (length (visible-frame-list)) 1)
          (delete-frame (selected-frame))
        (save-buffers-kill-emacs))
    (delete-frame (selected-frame)))
)
(global-set-key (kbd "C-x C-c") 'intelligent-close)

; save org buffers every 60 minutes
(run-at-time "00:59" 3600 'org-save-all-org-buffers)
(run-at-time "00:59" 1800  'org-mobile-pull)
(run-at-time "00:59" 1800  'org-mobile-push)

(projectile-global-mode)

(desktop-save-mode 1)

;;----------------- Keyboard settings
; Map command as control
(setq mac-command-modifier 'control)

; Commento with C-S-/ (C-?)
(global-set-key (kbd "C-?") 'comment-region)
(global-set-key (kbd "M-?") 'uncomment-region)
; Switch header/implementation with C-c o
(global-set-key (kbd "C-c o") 'ff-find-other-file)

; Add window navitagion to M
(global-set-key [M-left] 'windmove-left)          ; move to left windnow
(global-set-key [M-right] 'windmove-right)        ; move to right window
(global-set-key [M-up] 'windmove-up)              ; move to upper window
(global-set-key [M-down] 'windmove-down)          ; move to downer window

; git status
(global-set-key (kbd "M-g M-s")  `magit-status)

; NOTE: Experimental
(define-key read-expression-map [(tab)] 'hippie-expand)

;;----------------- ido mode
; turn on ido
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)
(add-hook 'ido-setup-hook
          (lambda ()
            (define-key ido-completion-map [tab] 'ido-complete)))

(global-set-key
 "\M-x"
 (lambda ()
   (interactive)
   (call-interactively
    (intern
     (ido-completing-read
      "M-x "
      (all-completions "" obarray 'commandp))))))

;; Set color theme
(load-theme 'tomorrow-night t)

; Clear whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;----------------- UI settings
; Disable toolbar
(tool-bar-mode -1)

; Enable menu-bar
(menu-bar-mode 1)
(setq ns-use-native-fullscreen t)

; Disable scroll-bar
; (scroll-bar-mode -1)

; always visually wrap lines
(global-visual-line-mode 1)

; Set font size in units of x/10 so :height 100 = 10 pt.

; Set cursor color and font lock
(set-cursor-color "#6785c5")
(global-font-lock-mode 1)

; Set column markers
(require 'column-marker)
(column-marker-1 72)
(column-marker-2 79)
(add-hook 'python-mode-hook (lambda () (interactive) (column-marker-1 72) (column-marker-2 79)))

;;----------------- yasnippet
;; set up yasnippet
(require 'yasnippet)
(setq yas-snippet-dirs '("~/.emacs.d/mysnippets"))
(yas/reload-all)
(yas-global-mode 1)

;;----------------- expand-region settings
(require 'expand-region)
(global-set-key (kbd "C-c C-d") 'er/expand-region)


;;---------------- Haskell settings
;; Taken from https://github.com/serras/emacs-haskell-tutorial/blob/master/tutorial.md#haskell-mode
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'turn-on-hi2)
(eval-after-load 'haskell-mode
  '(define-key haskell-mode-map [f8] 'haskell-navigate-imports))

(let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
  (setenv "PATH" (concat my-cabal-path ":" (getenv "PATH")))
  (add-to-list 'exec-path my-cabal-path))
(custom-set-variables '(haskell-tags-on-save t))

(custom-set-variables
  '(haskell-process-suggest-remove-import-lines t)
  '(haskell-process-auto-import-loaded-modules t)
  '(haskell-process-log t))
(eval-after-load 'haskell-mode '(progn
  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
  (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
  (define-key haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
  (define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info)
  (define-key haskell-mode-map (kbd "C-c C-n C-c") 'haskell-process-cabal-build)
  (define-key haskell-mode-map (kbd "C-c C-n c") 'haskell-process-cabal)
  (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)))
(eval-after-load 'haskell-cabal '(progn
  (define-key haskell-cabal-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
  (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
  (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)))
(custom-set-variables '(haskell-process-type 'cabal-repl))

(eval-after-load 'haskell-mode
  '(define-key haskell-mode-map (kbd "C-c C-o") 'haskell-compile))
(eval-after-load 'haskell-cabal
  '(define-key haskell-cabal-mode-map (kbd "C-c C-o") 'haskell-compile))

(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))

;;----------------- Sublime like settings
; Multiple cursors
(require 'multiple-cursors)

; Keyboard shortcuts for multiple-cursors. Maybe change to be like sublime (cmd + d)?
(global-unset-key (kbd "C-d"))
(global-unset-key (kbd "C-S-l"))
(global-set-key (kbd "C-d") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-S-l") 'mc/edit-lines)

(global-unset-key (kbd "C-c t"))

;;                  Gnus settings
(defun my-gnus-group-list-subscribed-groups ()
  "List all subscribed groups with or without un-read messages"
  (interactive)
  (gnus-group-list-all-groups 5)
  )
(add-hook 'gnus-group-mode-hook
          ;; list all the subscribed groups even they contain zero un-read messages
          (lambda () (local-set-key "o" 'my-gnus-group-list-subscribed-groups )))

;;                 Scala settings

;; (require `ensime)
;; (add-hook `scala-mode-hook `ensime-scala-mode-hook)

;;---------------- Markdown settings
(load "pandoc-mode")
(add-hook 'markdown-mode-hook 'pandoc-mode)

(require 'poly-R)
(require 'poly-markdown)
(add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode))

;; ------------------- helm Settingse

