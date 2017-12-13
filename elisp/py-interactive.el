(defun other/chomp (str)
   "Trim leading and trailing whitespace from STR."
   (replace-regexp-in-string "\\(\\`[[:space:]\n]*\\|[[:space:]\n]*\\'\\)" "" str))

(defun my/extract-variable (assignment-expression)
  "Extract the variable name from `assignment-expression'
(e.g. x = 3 -> x)"
  (interactive "MAssignment expression: ")
  (let* ((sub-exps (split-string assignment-expression " = "))
         (variable (car sub-exps))
         (variable (other/chomp variable)))
    variable))

(defun my/assignment-expression-p (expression)
  "Return true if `expression' is an assignment expression (e.g. x = 3)"
  (string-match-p (regexp-quote " = ") expression))

(defun my/get-line (line-number)
  (interactive "nLine number: ")
  (save-excursion
    (goto-line line-number)
    (mwim-beginning-of-code-or-line)
    (setq start (point))
    (mwim-end-of-code-or-line)
    (setq end (point))
    (kill-ring-save start end)
    (substring-no-properties (car kill-ring))))

(defun my/eval-expr (expr)
  "Create new code cell in a EIN worksheet and evaluate `expr' in it"
  (interactive "MExpression: ")
  (if (> (length expr) 0)
      (progn
        (message (concat "Evaluating " expr))
        (with-current-buffer "*edward*"
          (progn
            (ein:cell-set-text cell expr)
            (ein:worksheet-execute-cell ws cell))))
    (message "Skipping over blank expr!")))

(defun my/get-expr (line-number)
  "Return the line of text at `line-number' and expand it if it is an assignment expresssion for inspecting its value"
  (interactive "nLine number: ")
  (let ((line (my/get-line line-number)))
    (if (my/assignment-expression-p line)
        (let* ((variable (my/extract-variable line))
               (expr (concat line "\n" variable)))
          expr)
      line)))

(defun my/loop (&optional a b c)
  "Main Loop"
  (when (eq major-mode 'python-mode)
    (when (not (eq (line-number-at-pos) *next-line-number-to-eval*))
      (my/eval-buffer)
      (setq *next-line-number-to-eval* (line-number-at-pos)))
    (message (concat "*next line number to eval* = " (number-to-string *next-line-number-to-eval*)))))

;; (add-hook 'ein:connect-mode-hook
;;           (lambda ()
;;             (local-set-key (kbd "<C-return>") (quote my/ein:eval-current-line))))

;;; Global variables
(setq *next-line-number-to-eval* 1)
(add-hook 'after-change-functions 'my/loop)

;; (setq cell (ein:worksheet-get-current-cell :cell-p #'ein:codecell-p))
;; (setq ws (ein:worksheet--get-ws-or-error))

(setq print-level 1)
(setq print-length 1)
(setq print-circle t)

;; (setq table (make-hash-table))
;; (puthash 1 cell table)
;; (gethash 1 table)
;; (gethash 2 table)

;; (defun my/get-lines ()
;;   (interactive)
;;   (let ((buffer-string (buffer-substring-no-properties 1 (buffer-size))))
;;     (split-string buffer-string "\n")))

(defun my/clear-cells ()
  (with-current-buffer "*edward*"
    (beginning-of-buffer)
    (while t (setq not-done (call-interactively 'ein:worksheet-kill-cell)))))

(defun my/number-of-lines ()
  (save-excursion
    (end-of-buffer)
    (1- (line-number-at-pos))))

(defun my/eval-exprs (exprs)
  (with-current-buffer "*edward*"
    (dolist (expr exprs)
      (call-interactively 'ein:worksheet-insert-cell-below)
      (insert expr)
      (call-interactively 'ein:worksheet-execute-cell))))

(defun my/eval-buffer ()
  (interactive)
  (condition-case exception
      (my/clear-cells)
    ('error))
  (let* ((line-numbers (number-sequence 1 (my/number-of-lines)))
         (exprs (mapcar (lambda (line) (my/get-expr line)) line-numbers)))
    (my/eval-exprs exprs)))
