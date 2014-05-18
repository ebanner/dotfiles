;;;; Edward M. Banner
;
; Edward's emacs configurations
; Thu May 15 21:45:48 CDT 2014
 

;; `M-x electric-indent-mode' in just about every programming mode

; Use C-M-l instead of C-l sometimes to check out the difference

; Give up C-<Space> if we've gone and mapped it to Ctrl
(global-unset-key (kbd "C-SPC"))

; `M-o M-s' centers a line

; Have help mode behave a tiny bit more like info mode
(add-hook 'help-mode-hook (lambda ()
	    (define-key help-mode-map "l" 'help-go-back)))

; Enable some nice paren modes for programming languages
(setq prog-mode-hook (list (lambda ()
			     (show-paren-mode t))
			   (lambda ()
			     (electric-pair-mode t))
			   (lambda ()
			     (electric-indent-mode t))))


; Always turn auto fill mode on
(add-hook 'text-mode-hook 'auto-fill-mode)

; Add a nice highlighted line when viewing packages
(add-hook 'package-menu-mode-hook (lambda ()
				  (hl-line-mode t)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(menu-bar-mode t)
 '(next-screen-context-lines 5)
 '(package-archives (quote (("melpa" . "http://melpa.milkbox.net/packages/") ("gnu" . "http://elpa.gnu.org/packages/"))))
 '(scroll-conservatively 101)
 '(scroll-margin 1)
 '(sentence-end-double-space nil)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 111 :width normal :foundry "unknown" :family "Ubuntu Mono")))))
