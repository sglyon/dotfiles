;;; my AuCTeX Settings
; Make AUCTeX aware of style and multi file docs
; (add-to-list 'load-path "~/.emacs.d/elpa/auxtex-11.86")
; (load "auctex.el" nil t t)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

; Set up AUCTeX section command C-c C-s
(setq LaTeX-section-hook
                '(LaTeX-section-heading
            LaTeX-section-title
            LaTeX-section-toc
            LaTeX-section-section
            LaTeX-section-label))

; Turn RefTeX on for all .tex files
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode
(add-hook 'latex-mode-hook 'turn-on-reftex)   ; with Emacs latex mode
(setq reftex-plug-into-AUCTeX t)              ; use AUCTeX with RefTeX

; Set PATH variables so preview-latex mode can compile
(setq exec-path (append exec-path '("/usr/local/bin")))
(setenv "PATH" (concat "/usr/texbin:" (getenv "PATH")))
(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
