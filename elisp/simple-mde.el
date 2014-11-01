;;; FILLERS ;;;

(defconst *discourse-marker* "Discourse Marker")
(defconst *filled-pause* "Filled Pause")
(defconst *explicit-editing-term* "Explicit Editing Term")

;;; Fillers worthy of being marked
(defconst *discourse-response* "Discourse Response")
(defconst *aside/parenthetical* "Aside/Parenthetical")

;;; FILLERS ;;;

(defun mark-filler (type-of-filler)
  "Annotates the current word or region dictated by `type-of-filler'"
  (let (start end bounds)
    (if (use-region-p)
	(setq start (region-beginning)
	      end (region-end))
      (progn
	(setq bounds (bounds-of-thing-at-point 'symbol))
	(setq start (car bounds)
	      end (cdr bounds))))

    (cond ((equal type-of-filler *discourse-response*) ; /text/
	   (push-mark end)
	   (setq mark-active t)
	   (facemenu-set-italic))
	  ((equal type-of-filler *aside/parenthetical*)	; {text}
	   (goto-char end)
	   (insert "}")
	   (goto-char start)
	   (insert "{")
	   (goto-char start)
	   (setq end (+ end 2)))
	  (t (message type-of-filler)))
    (push-mark end)
    (setq mark-active t)
    (facemenu-set-bold)))

;;; Wrappers for `mark-filler' to pass to `define-key'

(defun discourse-marker () (interactive) (mark-filler *discourse-marker*))
(defun filled-pause () (interactive) (mark-filler *filled-pause*))
(defun explicit-editing-term () (interactive) (mark-filler *explicit-editing-term*))
(defun discourse-response () (interactive) (mark-filler *discourse-response*))
(defun aside/parenthetical () (interactive) (mark-filler *aside/parenthetical*))

;;; SUs ;;;
(defconst *statement* "Statement SU Break")
(defconst *question* "Question SU Break")
(defconst *backchannel* "Backchannel SU Break")
(defconst *incomplete* "Incomplete SU Break")
(defconst *coordination* "Coordination SU Break")
(defconst *clausal* "Clausal SU Break")

(defun insert-SU-break (type-of-SU)
  "Drops in an SU break dictated by `type-of-SU'"
  (let ((start (point))
	end)
    (cond ((equal type-of-SU *statement*)
	   (when (equal (char-after) 46) ; Period
	     (delete-char 1))
	   (insert " /.")
	   (setq start (+ start 1)))
	  ((equal type-of-SU *question*)
	   (when (equal (char-after) 63) ; Question Mark
	     (delete-char 1))
	   (insert " /?")
	   (setq start (+ start 1)))
	  ((equal type-of-SU *backchannel*) (insert "/@"))
	  ((equal type-of-SU *incomplete*) (insert "/-"))
	  ((equal type-of-SU *coordination*)
	   (insert " /&")
	   (setq start (+ start 1)))
	  ((equal type-of-SU *clausal*)
	   (when (equal (char-after) 44) ; Comma
	     (delete-char 1))
	   (insert " /,")
	   (setq start (+ start 1))))
    (goto-char start)
    (push-mark (+ start 2))
    (setq mark-active t)
    (forward-word)
    (backward-word)))

;;; Wrappers for `mark-SU' to pass to `define-key'

(defun statement ()
  (interactive)
  (insert-SU-break *statement*)
  (message "Statement SU Break"))

(defun question ()
  (interactive)
  (insert-SU-break *question*)
  (message "Question SU Break"))

(defun backchannel ()
  (interactive)
  (insert-SU-break *backchannel*)
  (message "Backchannel SU Break"))

(defun incomplete ()
  (interactive)
  (insert-SU-break *incomplete*)
  (message "Incomplete SU Break"))

(defun coordination ()
  (interactive)
  (insert-SU-break *coordination*)
  (message "Coordination SU Break"))

(defun clausal ()
  (interactive)
  (insert-SU-break *clausal*)
  (message "Clausal SU Break"))

(define-minor-mode simple-mde-mode
  "Minor mode for annotating a transcript with SimpleMDE convention"
  :lighter " SimpleMDE"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c d m") 'discourse-marker)
            (define-key map (kbd "C-c f p") 'filled-pause)
            (define-key map (kbd "C-c e e t") 'explicit-editing-term)
            (define-key map (kbd "C-c d r") 'discourse-response)
            (define-key map (kbd "C-c a p") 'aside/parenthetical)
            (define-key map (kbd "C-c .") 'statement)
            (define-key map (kbd "C-c ?") 'question)
            (define-key map (kbd "C-c b") 'backchannel)
            (define-key map (kbd "C-c i") 'incomplete)
            (define-key map (kbd "C-c &") 'coordination)
            (define-key map (kbd "C-c ,") 'clausal)
            map))

(provide 'simple-mde)