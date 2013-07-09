;; My ~/.emacs.d/init.el file
; based on: http://www.jesshamrick.com/2012/09/18/emacs-as-a-python-ide/

; Tell emacs about theme directory for colorschemes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (tomorrow-night)))
 '(custom-safe-themes (quote ("6dfcb4de19630ea3676c256ca3c648b43524364898d1b94adca536b10344fefd" default)))
 '(org-agenda-files (quote ("~/Dropbox/org/refile.org" "~/School/NYU/NYUclasses/Sargent/sargent.org")))
 '(org-directory "~/Dropbox/org")
 '(py-shell-local-path "~/anaconda/bin/ipython")
 '(py-shell-name "~/anaconda/bin/ipython")
 '(py-start-run-ipython-shell t)
 '(py-use-local-default t)
 '(python-shell-interpreter "~/anaconda/bin/ipython")
 '(safe-local-variable-values (quote ((eval ispell-change-dictionary "en_US") (eval org-expiry-deinsinuate)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-marker-1 ((t (:background "dark cyan"))))
 '(column-marker-2 ((t (:background "red3")))))
;;

; Add marmalade to package control sources
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                           ("marmalade" . "http://marmalade-repo.org/packages/")
                           ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

; ;- - - - - - - - - - - - - - - Global Settings - - - - - - - - - - - - - - - ;
;;; Sublime like settings
; Multiple cursors
(require 'multiple-cursors)

; Keyboard shortcuts for multiple-cursors. Maybe change to be like sublime (cmd + d)?
(global-unset-key (kbd "C-d"))
(global-unset-key (kbd "C-S-l"))
(global-set-key (kbd "C-d") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-S-l") 'mc/edit-lines)

; Clear whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

; Ensure new line at eof on save
(setq require-final-newline 't)

; Set column markers
(require 'column-marker)
(column-marker-1 72)
(column-marker-2 79)
(add-hook 'python-mode-hook (lambda () (interactive) (column-marker-1 72) (column-marker-2 79)))

; Map command as control
(setq mac-command-modifier 'control)

; Disable toolbar
(tool-bar-mode -1)

; Enable menu-bar
(menu-bar-mode 1)

; Disable scroll-bar
(scroll-bar-mode -1)

; Comment with C-S-/ (C-?)
(global-set-key (kbd "C-?") 'comment-region)
(global-set-key (kbd "M-?") 'uncomment-region)

; Switch header/implementation with C-c o
(global-set-key (kbd "C-c o") 'ff-find-other-file)

; always visually wrap lines
(global-visual-line-mode 1)

; Set defualt directory
(let ((default-directory "~/.emacs.d"))
  (normal-top-level-add-subdirs-to-load-path))
;; (add-to-list 'load-path "~/.emacs.d/plugins")
;; (add-to-list 'load-path "~/.emacs.d/elpa")

; Set font size in units of x/10 so :height 100 = 10 pt.
(set-face-attribute 'default nil :height 100)

; Set cursor color and font lock
(set-cursor-color "#6785c5")
(global-font-lock-mode 1)

; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)


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

; auto complete Settings
(add-to-list 'load-path "/Users/spencerlyon2/.emacs.d/plugins/autocomplete/")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "/Users/spencerlyon2/.emacs.d/plugins/autocomplete//ac-dict")
(ac-config-default)

; (add-to-list 'load-path "~/.emacs.d/plugins")
; (require 'fill-column-indicator)
; (define-globalized-minor-mode
;   global-fci-mode fci-mode (lambda () (fci-mode 1)))
; (global-fci-mode t)

; Add window navitagion to M
(global-set-key [M-left] 'windmove-left)          ; move to left windnow
(global-set-key [M-right] 'windmove-right)        ; move to right window
(global-set-key [M-up] 'windmove-up)              ; move to upper window
(global-set-key [M-down] 'windmove-down)          ; move to downer window

; expand-region settings
(require 'expand-region)
(global-set-key (kbd "C-c C-d") 'er/expand-region)

; ;; Set up google talk with Jabber
; (setq jabber-username "spencerlyon2")
;   (setq jabber-password "ly0no409")
;   (setq jabber-nickname "spencer")
;   (setq jabber-connection-type (quote ssl))
;   (setq jabber-network-server "talk.google.com")
;   (setq jabber-server "gmail.com")

; (setq jabber-account-list
;     '(("spencerlyon2@gmail.com"
;        (:network-server . "talk.google.com")
;        (:connection-type . ssl))))

;; load helm
; (require 'helm-config)

;; Projectile settings
; (projectile-global-mode)
; (setq projectile-use-native-indexing t)
; (setq projectile-enable-caching t)

;; project-persist settings
; (require 'project-persist)
; (require 'sr-speedbar)
; (project-persist-mode t)

;; linum-global mode
; (require 'linum-relative)
; (global-linum-mode)

;; git settings

(global-set-key (kbd "M-g M-s")  `magit-status)


; ;- - - - - - - - - - - - - - Mode-Specific Settings - - - - - - - - - - - - -;
(load-file "~/.emacs.d/python-setup.el")
(load-file "~/.emacs.d/org-setup.el")
(load-file "~/.emacs.d/tex-setup.el")


; ;- - - - - - - - - - - - - - - - - el-get settings - - - - - - - - - - - - -;
; (add-to-list 'load-path "~/.emacs.d/el-get/el-get")

; (unless (require 'el-get nil 'noerror)
;   (with-current-buffer
;       (url-retrieve-synchronously
;        "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
;     (goto-char (point-max))
;     (eval-print-last-sexp)))

; (add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
; (el-get 'sync)
