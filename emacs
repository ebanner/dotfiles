;;; Edward's emacs configuration file
;;; 
;;; Tue Dec  2 21:28:41 CST 2014

;;; Package management

(if (string= "lord-yupa.cs.utexas.edu" system-name)
    (progn  (add-to-list 'load-path "~/.emacs.d/lisp")
	    (add-to-list 'load-path "~/.emacs.d/elisp")
	    (require 'package)
	    (package-initialize))
  (progn (require 'package)
	 (package-initialize)
	 (add-to-list 'load-path "~/.emacs.d/lisp")))

;;; Add custom elisp code
(dolist (dir  '("lisp" "elisp"))
  (add-to-list 'load-path (concat "~/.emacs.d/" dir)))

;;; Grading mode
;(add-to-list 'load-path "~/.emacs.d/elisp")
;; (require 'grading)
;; (add-hook 'org-mode-hook
;; 	  (lambda ()
;; 	    (when (string= (file-name-nondirectory (buffer-file-name)) "rubric.org")
;; 	      (grading-mode))))

;;; Visual
(tool-bar-mode -1)
(scroll-bar-mode -1)

;;; Scrolling
(setq scroll-margin 2)
(setq scroll-step 1)

;;; Incremental search tweak
(add-hook 'isearch-mode-end-hook 'my-goto-match-beginning)
(defun my-goto-match-beginning ()
  (when (and isearch-forward isearch-other-end (not isearch-mode-end-hook-quit))
    (goto-char isearch-other-end)))
(defadvice isearch-exit (after my-goto-match-beginning activate)
  "Go to beginning of match."
  (when (and isearch-forward isearch-other-end)
    (goto-char isearch-other-end)))

;;; Programming
(add-hook 'prog-mode-hook (lambda () (electric-indent-mode 1)))
(add-hook 'prog-mode-hook (lambda () (whole-line-or-region-mode 1)))
(add-hook 'prog-mode-hook (lambda () (show-paren-mode 1)))

;;; Text Mode
(add-hook 'text-mode-hook (lambda () (auto-fill-mode 1)))
(add-hook 'text-mode-hook (lambda () (flyspell-mode 1)))
(add-hook 'text-mode-hook (lambda () (whole-line-or-region-mode 1)))

;;; Scroll up and down buffer
(global-set-key (kbd "M-n") 'scroll-up-line)
(global-set-key (kbd "M-p") 'scroll-down-line)

;;; Use `ibuffer' instead of `list-buffers'
(global-set-key (kbd "C-x C-b") 'ibuffer)

;;; Magit
(global-set-key (kbd "C-c m") 'magit-status)

;;; Docview mode reload PDFs automatigically when they change on disk
(add-hook 'doc-view-mode-hook 'auto-revert-mode)

;;; Registers for jumping to files
(set-register ?e '(file . "~/.emacs"))

;;; ido
(require 'ido)
(ido-mode t)

(add-hook 'minibuffer-setup-hook 'conditionally-enable-paredit-mode)
(defun conditionally-enable-paredit-mode ()
  "enable paredit-mode during eval-expression"
  (if (eq this-command 'eval-expression)
      (paredit-mode 1)))

;;; Paredit
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             'enable-paredit-mode)
(add-hook 'lisp-mode-hook             'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
(add-hook 'scheme-mode-hook           'enable-paredit-mode)
(add-hook 'minibuffer-inactive-mode-hook  'enable-paredit-mode)

;;; org mode
(require 'org-mouse)
(setq org-capture-templates
  '(("r" "Research"
         entry (file+datetree "~/Journal/research.org")
         "* %?"
         :empty-lines 1)
    ("m" "miscellaneous"
         entry (file+datetree "~/Journal/misc.org")
         "* %?"
         :empty-lines 1)
    ("t" "teaching"
         entry (file+datetree "~/Journal/teaching.org")
         "* %?"
         :empty-lines 1)
    ("a" "autonomous robots"
         entry (file+datetree "~/Journal/autonomous-robots.org")
         "* %?"
         :empty-lines 1)))
(add-hook 'org-mode-hook 'org-hide-block-all)
(defun zin/org-cycle-current-headline ()
  (interactive)
  (outline-previous-heading)
  (org-cycle))
(setq org-todo-keywords
      '((sequence "TODO" "WORKING" "|" "DONE")))
(setq org-log-done 'time)
;; (add-hook 'org-mode-hook (lambda () (auto-fill-mode 1) (reftex-mode 1) (org-indent-mode -1)))
(setq org-default-notes-file "~/Dropbox/org/Notes.org")
(add-hook 'org-mode-hook 'turn-on-org-cdlatex)
;;; (org-babel-do-load-languages 'org-babel-load-languages '((python . t)))
;;; (add-to-list 'org-structure-template-alist '("T" "#+TITLE: ?" "<title>?</title>"))
;;; (add-to-list 'org-structure-template-alist '("A" "#+AUTHOR: Edward Banner\n?" "<author>\n?</author>"))
;;; (add-to-list 'org-structure-template-alist '("D" "#+DATE: ?" "<date>?</date>"))
(add-hook 'org-mode-hook
	  (lambda ()
	    (auto-fill-mode 1)
	    (define-key org-mode-map (kbd "C-c TAB") 'zin/org-cycle-current-headline)
	    (define-key org-mode-map (kbd "RET") 'org-return-indent)
	    (define-key org-mode-map (kbd "C-c TAB") 'zin/org-cycle-current-headline)
	    ;; (when (string= (file-name-nondirectory (buffer-file-name)) "rubric.org")
	    ;;   (grading-mode))
	    ))
(define-key global-map (kbd "C-c c") 'org-capture)

;;; Location-specific settings
(cond ((memq window-system '(mac ns))	; Mac
       (define-key key-translation-map (kbd "¥") (kbd "C-x"))
       (setq command-line-default-directory "/Users/ebanner")
       (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
       (setq exec-path (append exec-path '("/usr/local/bin")))
       (setenv "PATH" (concat (getenv "PATH") ":/usr/texbin"))
       (setq exec-path (append exec-path '("/usr/texbin")))
       (define-key key-translation-map (kbd "M-¥") (kbd "\\"))
       (define-key key-translation-map (kbd "M-|") (kbd "|"))
       (set-face-attribute 'default nil :height 100)
       (set-frame-size (selected-frame) 95 52)
       (set-face-attribute 'default nil
                :family "Inconsolata" :height 120 :weight 'normal))
      
      ((string= system-name "edward-All-Series") ; Home
       ;;; Make C-x and M-x easy to press
       (define-key key-translation-map (kbd "ESC") (kbd "C-x"))
       (set-face-attribute 'region nil :background "LightGoldenrod2")
       (set-frame-size (selected-frame) 87 53)
       ;; (add-to-list 'load-path "/usr/share/emacs/site-lisp/org/")
       (openwith-mode t)
       (setq openwith-associations (quote (("\\.pdf\\'" "atril" (file)) ("\\.\\(?:mpe?g\\|avi\\|wmv\\)\\'" "mplayer" ("-idx" file)))))
       ;; (require 'org)
       ;; (org-babel-load-file "~/.emacs.d/elisp/research-toolkit.org")
       ;; (setq org-latex-pdf-process
       ;; 	     '("latexmk -pdflatex='pdflatex -interaction nonstopmode' -pdf -bibtex -f %f"))
       ;; (org-babel-load-file "~/.emacs.d/elisp/org-ref.org")
       ;; (setq org-export-latex-format-toc-function 'org-export-latex-no-toc)
       ;; (setq org-ref-default-bibliography (quote ("~/Classes/CS386/Project/citations")))
       ;; (setq reftex-default-bibliography (quote ("~/Classes/CS386/Project/citations")
       ;;; Make universal argument easier to press
       (define-key key-translation-map (kbd "ESC") (kbd "C-u")))
      
      ((string-match "cs.utexas.edu" system-name) ; GDC
       (autopair-mode 1)
       (whole-line-or-region-mode 1)
       (show-paren-mode 1)
       ;; (define-key global-map (kbd "C-c c") 'compile)
       (define-key global-map (kbd "C-c r") 'recompile)
       (global-set-key (kbd "RET") 'newline-and-indent)
       (define-key key-translation-map [?\C-h] [?\C-?])
       (setq-default truncate-lines 1)
       (keyboard-translate ?\C-i ?\H-i)
       (global-set-key [?\H-i] 'help-command)
       (define-key key-translation-map (kbd "M-h") [?\H-h])
       (global-set-key [?\H-h] 'backward-kill-word)
       (global-set-key (kbd "M-i") 'mark-paragraph)
       (set-frame-size (selected-frame) 82 51)))

;;; Make C-x and M-x awesome to press
(define-key key-translation-map (kbd "ESC") (kbd "C-x"))
(define-key key-translation-map (kbd "C-<escape>") (kbd "M-x"))

;;; Scroll up and down buffer
(global-set-key (kbd "M-n") 'scroll-up-line)
(global-set-key (kbd "M-p") 'scroll-down-line)

;;; Use `ibuffer' instead of `list-buffers'
(global-set-key (kbd "C-x C-b") 'ibuffer)

;;; magit
(global-set-key (kbd "C-c m") 'magit-status)

;;; Docview mode reload PDFs automatigically when they change on disk
(add-hook 'doc-view-mode-hook 'auto-revert-mode)

;;; Registers for jumping to files
(set-register ?e '(file . "~/.emacs"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-PDF-mode t)
 '(TeX-electric-escape t)
 '(auto-revert-interval 1)
 '(compilation-auto-jump-to-first-error t)
 '(dired-dwim-target t)
 '(dired-isearch-filenames t)
 '(display-buffer-reuse-frames t)
 '(doc-view-continuous t)
 '(ecb-new-ecb-frame t)
 '(eclim-eclipse-dirs (quote ("/opt/eclipse")))
 '(ido-auto-merge-delay-time 2)
 '(nxml-sexp-element-flag t)
 '(org-export-with-email t)
 '(package-archives
   (quote
    (("gnu" . "http://elpa.gnu.org/packages/")
     ("melpa" . "http://melpa.milkbox.net/packages/"))))
 '(projectile-indexing-method (quote native))
 '(scroll-margin 2)
 '(search-whitespace-regexp nil)
 '(sentence-end-double-space nil)
 '(speedbar-default-position (quote left))
 '(speedbar-use-images nil)
 '(speedbar-verbosity-level 0)
 '(virtualenv-root "~")
 '(wdired-allow-to-change-permissions t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 110 :width normal :foundry "unknown" :family "Ubuntu Mono")))))
