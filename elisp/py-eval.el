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
            (execute-extended-command nil "ein:worksheet-insert-cell-above")
            (insert expr)
            (execute-extended-command nil "ein:worksheet-execute-cell")
            (beginning-of-buffer)
            (execute-extended-command nil "ein:worksheet-goto-next-input"))))
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
      (let ((expr (my/get-expr *next-line-number-to-eval*)))
        (my/eval-expr expr))
      (setq *next-line-number-to-eval* (line-number-at-pos)))
    (message (concat "*next line number to eval* = " (number-to-string *next-line-number-to-eval*)))))

(defun my/eval-current-line ()
  (interactive)
  (let ((expr (my/get-expr (line-number-at-pos))))
    (my/eval-expr expr)))

;;; Global variables
(setq *next-line-number-to-eval* 1)
(add-hook 'after-change-functions 'my/loop)
