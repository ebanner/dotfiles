(defun mark-filler (filler)
  "Annotates the current word or region dictated by `filler'"
  (let (start end bounds)
    (if (use-region-p)
	(setq start (region-beginning)
	      end (region-end))
      (progn
	(setq bounds (bounds-of-thing-at-point 'symbol))
	(setq start (car bounds)
	      end (cdr bounds))))

    (cond ((equal filler "delreg")	; [ text ]
	   (progn (goto-char end)
		  (insert " ]")
		  (goto-char start)
		  (insert "[ ")
		  (goto-char start)))
	  ((equal filler "corrected-disfluency") (facemenu-set-underline)) ; _text_
	  ((equal filler "filler") ; *text*
	   (push-mark end)
	   (setq mark-active t)
	   (facemenu-set-bold))
	  ((equal filler "discourse-response") (facemenu-set-bold) (facemenu-set-italic)) ; */text/*
	  ((equal filler "aside/parenthetical")	; *{text}*
	   (goto-char end)
	   (insert "}")
	   (goto-char start)
	   (insert "{")
	   (goto-char start)
	   (push-mark (+ end 2))
	   (setq mark-active t)
	   (facemenu-set-bold))
	  ((equal filler "emphasize") (facemenu-set-italic)) ; /text/
	  (t (message filler)))))

;;; Wrappers for `mark-filler' to pass to `define-key'
(defun delreg () (interactive) (mark-filler "delreg"))
(defun corrected-disfluency () (interactive) (mark-filler "corrected-disfluency"))
(defun filler () (interactive) (mark-filler "filler"))
(defun discourse-response () (interactive) (mark-filler "discourse-response"))
(defun aside/parenthetical () (interactive) (mark-filler "aside/parenthetical"))

(define-minor-mode simple-mde-mode
  "Minor mode for annotating a transcript with SimpleMDE convention"
  :lighter " SimpleMDE"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c d e") 'delreg)
            (define-key map (kbd "C-c c d") 'corrected-disfluency)
            (define-key map (kbd "C-c f") 'filler)
            (define-key map (kbd "C-c d r") 'discourse-response)
            (define-key map (kbd "C-c a p") 'aside/parenthetical)
            map))

(provide 'simple-mde)
