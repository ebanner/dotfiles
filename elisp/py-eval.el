(defun my/chomp (str)
   "Trim leading and trailing whitespace from STR."
   (replace-regexp-in-string "\\(\\`[[:space:]\n]*\\|[[:space:]\n]*\\'\\)" "" str))

(defun my/eval-line ()
  "Evaluate the current line"
  ;; (ein:connect-eval-buffer)
  (let* ((line (thing-at-point 'line t))
         (line (my/chomp line)))
    (ein:shared-output-eval-string line)
    (sit-for 0.1)))

(defun my/get-output ()
  (with-current-buffer "*ein:shared-output*"
    (let* ((output-text (buffer-substring-no-properties 1 (buffer-size)))
           (lines (split-string output-text "\n")))
      (nth 3 lines))))

(defun my/put-output (output line-number)
  "Evaluate a line of python and write its value to the *edward* buffer"
  (get-buffer-create "*edward*")
  (with-current-buffer "*edward*"
    (progn (goto-line line-number)
           (kill-line)
           (insert output))))

(defun my/do-eval ()
  (interactive)
  (my/eval-line)
  (setq output (my/get-output))
  (setq line-number (line-number-at-pos))
  (print "output")
  (print output)
  (my/put-output output line-number))

(defun my/send-line ()
  (interactive)
  (save-excursion
    (mwim-beginning-of-code-or-line)
    (set-mark (point))
    (mwim-end-of-line-or-code)
    (call-interactively 'kill-ring-save))
  (with-current-buffer "*edward*"
    (progn
      (execute-extended-command nil "ein:worksheet-insert-cell-above")
      (yank)
      (execute-extended-command nil "ein:worksheet-execute-cell"))))
(defun my/chomp (str)
   "Trim leading and trailing whitespace from STR."
   (replace-regexp-in-string "\\(\\`[[:space:]\n]*\\|[[:space:]\n]*\\'\\)" "" str))

(defun my/eval-line ()
  "Evaluate the current line"
  ;; (ein:connect-eval-buffer)
  (let* ((line (thing-at-point 'line t))
         (line (my/chomp line)))
    (ein:shared-output-eval-string line)
    (sit-for 0.1)))

(defun my/get-output ()
  (with-current-buffer "*ein:shared-output*"
    (let* ((output-text (buffer-substring-no-properties 1 (buffer-size)))
           (lines (split-string output-text "\n")))
      (nth 3 lines))))

(defun my/put-output (output line-number)
  "Evaluate a line of python and write its value to the *edward* buffer"
  (get-buffer-create "*edward*")
  (with-current-buffer "*edward*"
    (progn (goto-line line-number)
           (kill-line)
           (insert output))))

(defun my/do-eval ()
  (interactive)
  (my/eval-line)
  (setq output (my/get-output))
  (setq line-number (line-number-at-pos))
  (print "output")
  (print output)
  (my/put-output output line-number))

(setq *my/state* 'unmodified)
(setq *my/line-number* nil)
(defun my/check (&optional a b c)
  (message "Hi there!")
  (message "Hi there!")
  (when (eq major-mode 'python-mode)
    (progn
      (message )
      (if (eq *my/state* 'unmodified)
          (progn
            (setq *my/state* 'modified)
            (setq *my/line-number* (line-number-at-pos))
            (message (concat "State is now " (symbol-name *my/state*))))
        (progn
          (when (not (eq (line-number-at-pos) *my/line-number*))
            (save-excursion
              (my/send-line *my/line-number*)
              (setq *my/state* 'unmodified)
              (message (concat "State is now " (symbol-name *my/state*))))))))))

(add-hook 'after-change-functions 'my/check)

(defun my/send-line (line-number &optional &rest a b c)
  (interactive)
  (save-excursion
    (goto-line line-number)
    (mwim-beginning-of-code-or-line)
    (setq start (point))
    (mwim-end-of-code-or-line)
    (setq end (point))
    (kill-ring-save start end))
  (with-current-buffer "*edward*"
    (progn
      (execute-extended-command nil "ein:worksheet-insert-cell-above")
      (yank)
      (execute-extended-command nil "ein:worksheet-execute-cell"))))
