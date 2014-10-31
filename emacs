;;; Edward's emacs configuration file

;;; Package management
(require 'package)

;;; Visual
(tool-bar-mode -1)
(scroll-bar-mode -1)

;;; Use `ibuffer' instead of `list-buffers'
(global-set-key (kbd "C-x C-b") 'ibuffer)

;;; Docview mode reload PDFs automatigically when they change on disk
(add-hook 'doc-view-mode-hook 'auto-revert-mode)

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

;;; Text Mode
(add-hook 'text-mode-hook (lambda () (auto-fill-mode 1)))
(add-hook 'text-mode-hook (lambda () (flyspell-mode 1)))
(add-hook 'text-mode-hook (lambda () (whole-line-or-region-mode 1)))

;;; org mode
(setq org-todo-keywords
      '((sequence "TODO" "WORKING" "|" "DONE")))
(setq org-log-done 'time)
(add-hook 'org-mode (lambda () (auto-fill-mode 1)))
(setq org-default-notes-file "~/Dropbox/org/Notes.org")
(define-key global-map "\C-cc" 'org-capture)
(add-hook 'org-mode-hook 'turn-on-org-cdlatex)
(org-babel-do-load-languages 'org-babel-load-languages '((python . t)))
(add-to-list 'org-structure-template-alist '("T" "#+TITLE: ?" "<title>?</title>"))
(add-to-list 'org-structure-template-alist '("A" "#+AUTHOR: Edward Banner\n?" "<author>\n?</author>"))
(add-to-list 'org-structure-template-alist '("D" "#+DATE: ?" "<date>?</date>"))

;;; Tags
(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (eshell-command 
   (format "find %s -type f -name \"*.py\" | etags -" dir-name)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-PDF-mode t)
 '(TeX-electric-escape t)
 '(auto-revert-interval 1)
 '(dired-isearch-filenames t)
 '(doc-view-continuous t)
 '(package-archives (quote (("gnu" . "http://elpa.gnu.org/packages/") ("melpa" . "http://melpa.milkbox.net/packages/"))))
 '(sentence-end-double-space nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(region ((t (:background "DarkGoldenrod1" :foreground "gtk_selection_fg_color")))))
(put 'narrow-to-region 'disabled nil)
