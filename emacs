;;; Edward's emacs configuration file

;;; Package management
(require 'package)
(package-initialize)

;;; Visual
(tool-bar-mode -1)
(scroll-bar-mode -1)

;;; Make universal argument easier to press
;; (define-key key-translation-map (kbd "ESC") (kbd "C-u"))

;;; Location-specific settings
(cond ((memq window-system '(mac ns))	; Mac
       (setq command-line-default-directory "/Users/ebanner")
       (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
       (setq exec-path (append exec-path '("/usr/local/bin")))
       (setenv "PATH" (concat (getenv "PATH") ":/usr/texbin"))
       (setq exec-path (append exec-path '("/usr/texbin")))
       (define-key key-translation-map (kbd "M-¥") (kbd "\\"))
       (define-key key-translation-map (kbd "M-|") (kbd "|"))
       (set-face-attribute 'default nil :height 100)
       (set-frame-size (selected-frame) 95 52))
      ((string= system-name "edward-All-Series") ; Home
       (set-face-attribute 'region nil :background "LightGoldenrod2")
       (set-frame-size (selected-frame) 87 53))
      ((string= system-name "infiniti.ischool.utexas.edu") ; iSchool
       (set-face-attribute 'default nil :height 110)
       (set-frame-size (selected-frame) 88 58)))

;;; Use `ibuffer' instead of `list-buffers'
(global-set-key (kbd "C-x C-b") 'ibuffer)

;;; magit
(global-set-key (kbd "C-c m") 'magit-status)

;;; Docview mode reload PDFs automatigically when they change on disk
(add-hook 'doc-view-mode-hook 'auto-revert-mode)

;;; Registers for jumping to files
(set-register ?e '(file . "~/.emacs"))

;;; Python
(add-hook 'python-mode-hook
	  (lambda ()
	    (define-key python-mode-map (kbd "RET") 'newline-and-indent)
	    (autopair-mode 1)
	    (electric-indent-mode nil)
	    (setq
	     python-shell-interpreter "ipython"
	     python-shell-prompt-regexp "In \\[[0-9]+\\]: "
	     python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
	     python-shell-completion-setup-code "from IPython.core.completerlib import module_completion"
	     python-shell-completion-module-string-code "';'.join(module_completion('''%s'''))\n"
	     python-shell-completion-string-code "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")))
(add-hook 'inferior-python-mode-hook (lambda () (autopair-mode 1)))

;;; Jedi
(require 'jedi)
(add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'inferior-python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)
(setq jedi:environment-root "jedi")  ; or any other name you like
(setq jedi:environment-virtualenv
      (append python-environment-virtualenv
              '("--python" "/usr/bin/python3")))
(setq jedi:server-args
      '("--sys-path" "/usr/lib/python3/dist-packages/"))

;;; Paredit
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)
(add-hook 'minibuffer-inactive-mode-hook  #'enable-paredit-mode)

;;; ido
(require 'ido)
(ido-mode t)

;;; Programming
(add-hook 'prog-mode-hook (lambda () (electric-indent-mode 1)))
(add-hook 'prog-mode-hook (lambda () (whole-line-or-region-mode 1)))
(add-hook 'prog-mode-hook (lambda () (show-paren-mode 1)))

;;; Text Mode
(add-hook 'text-mode-hook (lambda () (auto-fill-mode 1)))
(add-hook 'text-mode-hook (lambda () (flyspell-mode 1)))
(add-hook 'text-mode-hook (lambda () (whole-line-or-region-mode 1)))

;;; org mode
(defun zin/org-cycle-current-headline ()
  (interactive)
  (outline-previous-heading)
  (org-cycle))
(setq org-todo-keywords
      '((sequence "TODO" "WORKING" "|" "DONE")))
(setq org-log-done 'time)
(add-hook 'org-mode (lambda () (auto-fill-mode 1)))
(setq org-default-notes-file "~/Dropbox/org/Notes.org")
(add-hook 'org-mode-hook 'turn-on-org-cdlatex)
(org-babel-do-load-languages 'org-babel-load-languages '((python . t)))
(add-to-list 'org-structure-template-alist '("T" "#+TITLE: ?" "<title>?</title>"))
(add-to-list 'org-structure-template-alist '("A" "#+AUTHOR: Edward Banner\n?" "<author>\n?</author>"))
(add-to-list 'org-structure-template-alist '("D" "#+DATE: ?" "<date>?</date>"))
(add-hook 'org-mode-hook
	  (lambda ()
	    (auto-fill-mode 1)
	    (define-key org-mode-map (kbd "C-c TAB") 'zin/org-cycle-current-headline)))
(define-key global-map (kbd "C-c c") 'org-capture)

;;; SimpleMDE mode
(add-to-list 'load-path "~/.emacs.d/elisp")
(require 'simple-mde)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-PDF-mode t)
 '(TeX-electric-escape t)
 '(auto-revert-interval 1)
 '(dired-dwim-target t)
 '(dired-isearch-filenames t)
 '(doc-view-continuous t)
 '(jedi:tooltip-method nil)
 '(nxml-sexp-element-flag t)
 '(package-archives (quote (("gnu" . "http://elpa.gnu.org/packages/") ("melpa" . "http://melpa.milkbox.net/packages/"))))
 '(search-whitespace-regexp nil)
 '(sentence-end-double-space nil))
