;;; miscellaneous.el --- Insert org blocks  -*- lexical-binding:t -*-
;;; Commentary:
;; This file provides some jupyter-like "cell creation" functions.
;;; Code:
(require 'org)
;; source: https://www.emacswiki.org/emacs/misc-cmds.el
;; Recommendation: bind to C-c r
(defun revert-buffer-no-confirm ()
    "Revert buffer without confirmation."
    (interactive)
    (revert-buffer :ignore-auto :noconfirm))

(defun toggle-case-initial(d)
  "Toggles the case of the first letter of the word under the cursor."
  (interactive "d")
  ;; interactive means we can call this from the outside, "d" returns the cursor point as the
  ;; parameter d.
  (skip-chars-backward "a-zA-Z0-9")
  ;; we skip backwards from the point, over all letters/numbers.
  (let (p ch)
    ;; we create a scope for point p and ch and set them.
    (setq p (point))
    (setq ch (buffer-substring p (+ p 1)))
    ;; next is a conditional (like if elseif elseif...) that checks whether the character at the
    ;; beginning of the word is uppercase or lowercase, and switches this.
    (cond
     ((string= ch (upcase ch))
      (delete-char 1)
      (insert (downcase ch))
      ;; go back a char because insertion moves the point forward
      (backward-char 1))
     ((string= ch (downcase ch))
      (delete-char 1)
      (insert (upcase ch)))))
  ;; return cursor to the original point
  (goto-char d))

(defun set-fill-columns (c)
  "Set fill column to C globally, including python and whitespace."
  (interactive "nSet fill-column equivalents (globally) to: ")
  (setq-default fill-column c
                flycheck-flake8-maximum-line-length c)
  (setq whitespace-line-column c
        python-black-extra-args (list "-l" (format "%s" c))
        py-isort-options (list (format "-l=%s" c) "-m=3" "-tc" "-fgw=0" "-ca")))

(defun current-indent-size ()
 "Return horizontal position of first non-whitespace char on line.
The first position is 0."
 (interactive)
 (save-excursion
   (back-to-indentation)
   (current-column)))

;; Recommendation: bind to C-a
(defun beginning-of-indent-or-line ()
  "Move to beginning indent block or to beginning of line."
  (interactive)
  (let ((indent-size (current-indent-size)))
    (cond ((> (current-column) indent-size) (back-to-indentation))
          ((= (current-column) indent-size) (beginning-of-line))
          ((= (current-column) 0) (back-to-indentation))
          (t (beginning-of-line)))))

(defun org-src-rm-leading-columns (&optional col)
  "Remove first (COL, default 2) columns from a src block.
Use to fix old narratives where python blocks were indented."
  (interactive)
  (save-excursion
    (if (not (string-prefix-p "#+begin" (thing-at-point 'line) t))
        (org-backward-element))
    (forward-line)
    (let ((a (point)))
      (re-search-forward "#\\+end")
      (beginning-of-line)
      (backward-word)
      (move-to-column (or col 2))
      (delete-rectangle a (point)))))

(defun org-babel-toggle-results-below-point ()
  "Toggle visibility of results below point."
  (interactive)
  (save-excursion
    (org-babel-hide-result-toggle)
    (while (org-next-block 1)
      (org-babel-hide-result-toggle))))

;; https://emacs.stackexchange.com/questions/29304/how-to-show-all-contents-of-current-subtree-and-fold-all-the-other-subtrees#29306
(defun org-show-just-me (&rest _)
  "Fold all other trees, then show entire current subtree."
  (interactive)
  (org-overview)
  (org-reveal)
  (org-show-subtree))

(defun org-show-next (&rest _)
  "Fold all other trees, then show entire current subtree."
  (interactive)
  (org-next-visible-heading 1)
  (org-reveal)
  (org-cycle)
  (recenter-top-bottom 0))

(defun org-toggle-emphasis ()
  "Toggle hiding/showing of org emphasize markers."
  (interactive)
  (font-lock-mode)
  (if org-hide-emphasis-markers
      (set-variable 'org-hide-emphasis-markers nil)
    (set-variable 'org-hide-emphasis-markers t))
  (font-lock-mode))


(defun upcase-rectangle (b e)
  "Change chars in rectangle between B and E to uppercase."
  (interactive "r")
  (apply-on-rectangle 'upcase-rectangle-line b e))

(defun upcase-rectangle-line (startcol endcol)
  (when (= (move-to-column startcol) startcol)
    (upcase-region (point)
                   (progn (move-to-column endcol 'coerce)
                          (point)))))

(defun downcase-rectangle (b e)
  "Change chars in rectangle between B and E to lowercase."
  (interactive "r")
  (apply-on-rectangle 'downcase-rectangle-line b e))

(defun downcase-rectangle-line (startcol endcol)
  (when (= (move-to-column startcol) startcol)
    (downcase-region (point)
                     (progn (move-to-column endcol 'coerce)
                            (point)))))

(defun copy-comment-region ()
  "Call (kill-ring-save) and (comment-region) together."
  (interactive)
  (call-interactively 'kill-ring-save)
  (call-interactively 'comment-region))


(provide 'miscellaneous)
;;; miscellaneous.el ends here
