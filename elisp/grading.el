;;; grading-mode.el -- Minor mode for manipulating rubrics
;;; Copyright (C) 2015, Edward Banner <ebanner@cs.utexas.edu>

;; modern-web-apps-grading-mode.el is free software; you can
;; redistribute it and/or modify it under the terms of the GNU General
;; Public License as published by the Free Software Foundation; either
;; version 2 of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA


(defun grading-grade ()
  "Grade a student's rubric based on check marks and point values"
  (interactive)
  
  ;; Helper functions
  (defun check-mark-p (c)
  "Is the check mark checked?"
  (char-equal c ?\X))
  
  (defun char-to-int (c)
    "Get the integer value of the character at point"
    (- c 48))

  ;; Body
  (save-excursion
    (beginning-of-buffer)
    
    ;; Reinitialize the first line with a fresh "Grade: "
    (reinit-first-line)
    
    ;; Tally up score by counting check marks
    (let ((points 0)
	  (total 0))
      (while (re-search-forward "\\[\\( \\|X\\)\\] (./.)" nil t)
	(backward-char 2)
	(let ((worth (char-to-int (char-after))))
	  (backward-char 2)
	  (let ((credit (char-to-int (char-after))))
	    (backward-char 4)
	    (if (check-mark-p (char-after)) ; Got credit
		(setq points (+ points credit)))
	    (setq total (+ total worth)))))
      (beginning-of-buffer)
      (move-end-of-line nil)
      (insert (format "%.0f" (* 100 (/ (float points) total)))))))

(defun grading-convert-rubric ()
  "Convert old-style rubrics that didn't account for partial credit"
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (while (re-search-forward "(3)" nil t)
      (replace-match "(3/3)"))
    (beginning-of-buffer)
    (while (re-search-forward "(2)" nil t)
      (replace-match "(2/2)"))))

(defun grading-init-rubric ()
  "Checks all check boxes and inserts grade"
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (while (re-search-forward "\\[ \\]" nil t)
      (replace-match "[X]")))
  (reinit-first-line))

(defun reinit-first-line ()
  (save-excursion)
  (beginning-of-buffer)
  (org-kill-line)
  (insert "Grade: "))


(define-minor-mode grading-mode
  "Minor mode for performing operations with rubrics for students demoing code in CS378 - Modern Web Applications"
  :lighter " Grading"
  :keymap (let ((map (make-sparse-keymap)))
	    (define-key map (kbd "C-c i") 'grading-init-rubric)
            (define-key map (kbd "C-c g") 'grading-grade)
            (define-key map (kbd "C-c c") 'grading-convert-rubric)
            map))

(provide 'grading)