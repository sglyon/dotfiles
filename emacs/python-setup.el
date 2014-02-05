;; My custom python setup

; (add-to-list 'load-path "~/.emacs.d/plugins/python-mode.el-6.1.0/")
; (setq py-install-directory "~/.emacs.d/plugins/python-mode.el-6.1.0/")
(require 'python-mode)

; use IPython
(setq py-shell-name "/usr/local/anaconda/bin/ipython")  ; if problem set full path
(setq-default py-which-bufname "IPython")
(setq py-python-command-args
  '("--gui=osx" "--pylab=osx" "-colors" "Linux"))
(setq py-force-py-shell-name-p t)

; switch to the interpreter after executing code
(setq py-shell-switch-buffers-on-execute-p t)
(setq py-switch-buffers-on-execute-p t)

; don't split windows
(setq py-split-windows-on-execute-p nil)
; try to automagically figure out indentation
(setq py-smart-indentation t)

(setq ipython-command "/usr/local/anaconda/bin/ipython")
(require 'ipython)

(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))
(setq py-python-command-args '("--colors=linux"))
(defun python-use-ipython (cmd args)
  (setq ipython-command cmd)
  (setq py-python-command-args args)
  )

(setq ipython-completion-command-string "print(';'.join(get_ipython().Completer.complete('%s')[1])) #PYTHON-MODE SILENT\n")

; Pymacs
; (autoload 'pymacs-apply "pymacs")
; (autoload 'pymacs-call "pymacs")
; (autoload 'pymacs-eval "pymacs" nil t)
; (autoload 'pymacs-exec "pymacs" nil t)
; (autoload 'pymacs-load "pymacs" nil t)
; (autoload 'pymacs-autoload "pymacs")


; ; ropemacs
; (require 'pymacs)
; (pymacs-load "ropemacs" "rope-")


(provide 'python-setup)
