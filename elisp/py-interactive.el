(defun other/chomp (str)
  "Trim leading and trailing whitespace from STR."
  (replace-regexp-in-string "\\(\\`[[:space:]\n]*\\|[[:space:]\n]*\\'\\)" "" str))

(defun my/loop (&optional a b c)
  "Main Loop"
  (when (string= (buffer-name) "*client*")
    (message "Change at %S!" (list a b c))
    (message "Change is %S!" (buffer-substring-no-properties a b))
    (message "Change has a length of %S" c)
    (let ((line-no (line-number-at-pos)))
      (when (not (eq line-no *next-line-number-to-eval*))
        (setq *next-line-number-to-eval* line-no)
        (my/do-process)))
    (message (concat "*next line number to eval* = " (number-to-string *next-line-number-to-eval*)))))

;; (defun my/ein:eval-current-line ()
;;   (interactive)
;;   (setq start (point))
;;   (when (not mark-active)
;;     (beginning-of-line)
;;     (set-mark (point))
;;     (end-of-line))
;;   (call-interactively 'ein:connect-eval-region)
;;   (setq mark-active nil)
;;   (goto-char start))

;; (add-hook 'ein:connect-mode-hook
;;           (lambda ()
;;             (local-set-key (kbd "<C-return>") (quote my/ein:eval-current-line))))

;;; Global variables
(setq *next-line-number-to-eval* 1)

;;; prevent emacs from printing out recursive data structures
(setq print-level 1)
(setq print-length 10)
(setq print-circle t)

(defun my/clear-cells ()
  (interactive)
  (with-current-buffer "*edward*"
    (beginning-of-buffer)
    (condition-case exception
        (while t (call-interactively 'ein:worksheet-kill-cell))
      ('error))))

(defun my/make-code-cell-and-eval (expr)
  (interactive "MExpression: ")
  (message "Inserting: %S" expr)
  (with-current-buffer "*edward*"
    (end-of-buffer)
    (call-interactively 'ein:worksheet-insert-cell-below)
    (insert expr)
    (call-interactively 'ein:worksheet-execute-cell)))

;;; elisp server
(require 'cl)
(require 'epcs)
(epcs:server-start
 (lambda (mngr)
   (lexical-let ((mngr mngr))
     (epc:define-method
      mngr 'make-code-cell-and-eval
      (lambda (&rest args)
        (let ((expr (car args)))
          (message "MAKE-CODE-CELL-AND-EVAL got %S" args)
          (message "TYPE = %S" (type-of args))
          (my/make-code-cell-and-eval expr)
          nil)))))
 9999)

(defun my/buffer-string ()
  (save-excursion
    (end-of-buffer)
    (buffer-substring-no-properties 1 (point))))

;;; python server
(require 'epc)
(setq my-epc (epc:start-epc "python" '("ast-server.py")))
(defun my/annotate-make-cells-eval (code)
  (deferred:$
    (epc:call-deferred my-epc 'annotate `(,code))
    (deferred:nextc it
      (lambda (annotated-code)
        (message "Annotated code: %S" annotated-code)
        (ein:shared-output-eval-string annotated-code)))
    (deferred:error it
      (lambda (err)
        (cond
         ((stringp err) (message "Error is %S" err))
         ((eq 'epc-error (car err)) (message "Error is %S" (cadr err))))))))

(defun defun-p (code)
  (let* ((lines (split-string code "\n"))
         (nonempty-lines (seq-filter (lambda (line) (> (length line) 0)) lines))
         (first-line (other/chomp (car nonempty-lines))))
    (string-prefix-p "def" first-line)))

(defun my/get-code ()
  (interactive)
  (save-excursion
    (python-mark-defun)
    (let ((code (buffer-substring-no-properties (region-beginning) (region-end))))
      (call-interactively 'kill-ring-save)
      (if (defun-p code)
          code
        (my/buffer-string)))))

(defun my/do-process ()
  (interactive)
  (let ((code (my/get-code)))
    (message "Doing code = %S" code)
    (my/clear-cells)
    (my/annotate-make-cells-eval code)))

(add-hook 'after-change-functions 'my/loop)
