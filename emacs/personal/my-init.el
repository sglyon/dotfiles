; Tell emacs about theme directory for colorschemes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")


 ;;----------------- General Settings
(setq user-email-address "sgl290@stern.nyu.edu")
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

; save org buffers every 6 minutes
(run-at-time "00:59" 3600 'org-save-all-org-buffers)


(projectile-global-mode)

(desktop-save-mode 1)

;;----------------- Keyboard settings
; Map command as control
(setq mac-command-modifier 'control)

; Comment with C-S-/ (C-?)
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

; Disable scroll-bar
(scroll-bar-mode -1)

; always visually wrap lines
(global-visual-line-mode 1)

; Set font size in units of x/10 so :height 100 = 10 pt.
(set-face-attribute 'default nil :height 100)

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
(yas-reload-all)
