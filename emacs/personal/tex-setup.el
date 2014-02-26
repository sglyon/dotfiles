;; Make AUCTeX aware of style and multi file docs
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

; Set PATH variables so preview-latex mode can compile
(setq exec-path (append exec-path '("/usr/local/bin")))
(setenv "PATH" (concat "/usr/texbin:" (getenv "PATH")))
(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))

;; Set up AUCTeX section command C-c C-s
(setq LaTeX-section-hook
      '(LaTeX-section-heading
    LaTeX-section-title
    LaTeX-section-toc
    LaTeX-section-section
    LaTeX-section-label))

;; Turn RefTeX on for all .tex files
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode
(add-hook 'latex-mode-hook 'turn-on-reftex)   ; with Emacs latex mode
(setq reftex-plug-into-AUCTeX t)              ; use AUCTeX with RefTeX

;; turn on cdlatex mode
(add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)   ; with AUCTeX LaTeX mode
(add-hook 'latex-mode-hook 'turn-on-cdlatex)   ; with Emacs latex mode


;; ;; (autoload 'ebib "ebib" "Ebib, a BibTeX database manager." t)
(require 'tex-site)

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(add-to-list 'auto-mode-alist '("\\.pdf\\'" . doc-view-mode))
(setq TeX-PDF-mode t)

(add-hook 'doc-view-mode-hook 'auto-revert-mode)

 ;;set xetex mode in tex/latex
(add-hook 'LaTeX-mode-hook (lambda()
(add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))
(add-to-list 'TeX-command-list '("PdfLaTeX" "%`pdflatex -synctex=1%(mode)%' %t" TeX-run-TeX nil t))
(setq TeX-command-default "LaTeX")
(setq TeX-save-query nil)
(setq TeX-show-compilation nil)
))
;; if you do not want XeLaTeX to be your default TeX command, follow the directions for for Mac OS X, 3.B., below, only replace the command "%`xelatex --synctex=1%(mode)%' %t" with "%`xelatex%(mode)%' %t".]
;;     B. On Aquamacs or Carbon Emacs, do the following (taken from directions here):

;;         i) Open a .tex buffer
;;         ii) Go to the following LaTeX menu: "LaTeX > Customize AUCTeX > Extend this Menu"
;;         iii) In this expanded menu, choose "TeX Command > "TeX Command List..."
;;         iv) Click on "INS" to create a new entry
;;         v) Name: XeLaTeX
;;         vi) Command: %`xelatex --synctex=1%(mode)%' %t
;;         vii) Click in the "Modes" button and choose "Value Menu > Set"
;;         viii) Check the boxes for: LaTeX, ConTeXt, AmSTeX
;;         ix) Save the settings

;;     Now you can run XeLaTeX through the menu "Command > XeLaTeX".
;; if you do not want XeLaTeX to be your default TeX command, follow the directions for for Mac OS X, 3.B., below, only replace the command "%`xelatex --synctex=1%(mode)%' %t" with "%`xelatex%(mode)%' %t".]

(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
;; (setq TeX-view-program-list '("zathura"))
(load "lorem-ipsum.el")
(require 'lorem-ipsum)

;; (setq reftex-default-bibliography '("~/git/research/refs"))

(autoload 'cdlatex-mode "cdlatex" "CDLaTeX Mode" t)
(setq cdlatex-math-symbol-alist
      '((?.
     ("\\cdot" "\\dot{?}" "\\ddot{?}"))
    (?v
     ("\\vee" "\\vec{?}" ""))
    (?m
     ("\\mu" "\\mat{?}" ""))

    )
      )
(setq cdlatex-env-alist
      '(("align" "\\begin{align}\nAUTOLABEL\n?\n\\end{align}\n" nil)
        ("als" "\\begin{align*}\n\t?\n\\end{align*}" nil)
    ("inlineeq" "\\[?\\]" nil)
    ("mathcal" "\\mathcal{?}" nil)
    ("partial" "\\frac{\\partial ?}{\\partial }" nil)
    ("breqn" "\\begin{dmath}\nAUTOLABEL\n?\n\\end{dmath}\n" nil)))

(setq cdlatex-command-alist
      '(("ieq" "Insert inline equation env"   "" cdlatex-environment ("inlineeq") t nil)
    ("beq" "Insert breqn equation env"   "" cdlatex-environment ("breqn") t nil)
    ("\(" "Insert inline math env"   "\(?\)" cdlatex-position-cursor nil nil t)
    ("cal" "Insert mathcal term"   "" cdlatex-environment ("mathcal") t nil)
    ("par" "Insert partial" "\\frac{\\partial ?}{\\partial }" cdlatex-position-cursor nil nil t)
    ("dif" "Insert diff" "\\frac{d ?}{d }" cdlatex-position-cursor nil nil t)
    ("al" "Insert align env" "" cdlatex-environment ("align") t nil)))

;; (autoload 'turn-on-cdlatex "cdlatex" "CDLaTeX Mode" nil)

(defun reftex-format-cref (label def-fmt)
  (format "\\cref{%s}" label))
(setq reftex-format-ref-function 'reftex-format-cref)

;; make latexmk available via C-c C-c
;; Note: SyncTeX is setup via ~/.latexmkrc (see below)
(add-hook 'LaTeX-mode-hook (lambda ()
  (push
    '("latexmk" "latexmk -pdf %s" TeX-run-TeX nil t
      :help "Run latexmk on file")
    TeX-command-list)))
(add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "PdfLaTeX")))

;; use Skim as default pdf viewer
;; Skim's displayline is used for forward search (from .tex to .pdf)
;; option -b highlights the current line; option -g opens Skim in the background
(setq TeX-view-program-selection '((output-pdf "PDF Viewer")))
(setq TeX-view-program-list
     '(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))

(defun my-latex-setup ()
  (defun latex-word-count ()
    (interactive)
    (let* ((this-file (buffer-file-name))
           (word-count
            (with-output-to-string
              (with-current-buffer standard-output
                (call-process "texcount" nil t nil "-brief" this-file)))))
      (string-match "\n$" word-count)
      (message (replace-match "" nil nil word-count))))
    (define-key LaTeX-mode-map "\C-cw" 'latex-word-count))
(add-hook 'LaTeX-mode-hook 'my-latex-setup t)

(setq reftex-label-alist
      '(("axiom"   ?a "ax:"  "~\\ref{%s}" nil ("axiom"   "ax.") -2)
    ("theorem" ?h "thm:" "~\\ref{%s}" t   ("theorem" "th.") -3)
    ("lemma" ?l "lemma:" "~\\ref{%s}" t   ("lemma" "l.") -4)))
