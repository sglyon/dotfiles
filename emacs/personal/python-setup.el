;; My custom python setup
(setq load-path (cons "~/src/Emacs/python-mode" load-path))
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

(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))
(setq py-python-command-args '("--colors=linux"))
(defun python-use-ipython (cmd args)
  (setq ipython-command cmd)
  (setq py-python-command-args args)
  )

