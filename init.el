;;; Coffeemacs --- Emacs config on Windows by Whiskeybear
;;; Commentary:
;;; Have a look at coffeemacs.org for the main customization.
;;; This file only installs packages and loads the code in coffeemacs.org and personal.el

;;; Code:
;; Setup package managers
(require 'package)
(defvar personal-file (locate-user-emacs-file "personal.el")
  "Path to the personal.el file in coffeemacs.")
(load personal-file)

(setq package-archives
      '(("gnu elpa" . "https://elpa.gnu.org/packages/")
        ("melpa"    . "https://melpa.org/packages/"))
      package-archive-priorities
      '(("melpa"    . 6)
        ("gnu elpa" . 5)))
(package-initialize)

;; Run package-refresh-contents if it hasn't been done for a week
(let* ((file "~/.emacs.d/.last-pkg-refresh")
       (day-sec (* 7 24 3600))
       (last (if (file-exists-p file) (with-temp-buffer (insert-file-contents file) (read (current-buffer))) 0)))
  (when (> (- (float-time) last) day-sec)
    (package-refresh-contents)
    (with-temp-file file (print (float-time) (current-buffer)))))

;; Setup use-package (although this is now included in Emacs 29)
(if (not (package-installed-p 'use-package))
    (progn (package-install 'use-package)))

;; Loading coffeemacs.org, which is the main customization file.
(prefer-coding-system 'utf-8-unix) ;; To remove the prompt during tangling
(let* ((org-file (expand-file-name "coffeemacs.org" user-emacs-directory))
       (el-file (expand-file-name "coffeemacs.el" user-emacs-directory)))
  ;; Check if coffeemacs.el exists and is older than coffeemacs.org
  (when (or (not (file-exists-p el-file))
            (file-newer-than-file-p org-file el-file))
    ;; Tangle coffeemacs.org to coffeemacs.el if .el doesn't exist or is older
    (require 'ob-tangle)
    (org-babel-tangle-file org-file el-file))
  ;; Load the .el file regardless of the tangling
  (load-file el-file))


;;; init.el ends here
