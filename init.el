;;; Coffeemacs --- Emacs config on Windows by Whiskeybear
;;; Commentary:
;;; Have a look at coffeemacs.el for the main customization.
;;; This file only installs packages, loads coffeemacs.el

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
(package-refresh-contents)

;; Setup use-package (although this is now included in Emacs 29)
(if (not (package-installed-p 'use-package))
    (progn (package-install 'use-package)))

;; Jupyter for Python
(if (not (package-installed-p 'jupyter))
    (progn (package-install 'jupyter)))

;; Doom-themes
(if (not (package-installed-p 'doom-themes))
    (progn (package-install 'doom-themes)))

;; Solarized themes
(if (not (package-installed-p 'solarized-theme))
    (progn (package-install 'solarized-theme)))

;; Anzu
(if (not (package-installed-p 'anzu))
    (progn (package-install 'anzu)))

;; Vertico
(if (not (package-installed-p 'vertico))
    (progn (package-install 'vertico)))

;; Orderless - Vertico compliment
(if (not (package-installed-p 'orderless))
    (progn (package-install 'orderless)))

;; Remove unused or add missing python imports
(if (not (package-installed-p 'pyimport))
    (progn (package-install 'pyimport)))

;; Black (Make sure to pip install black)
(if (not (package-installed-p 'python-black))
    (progn (package-install 'python-black)))

;; Crux
(unless (package-installed-p 'crux)
  (package-install 'crux))

;; Json-mode
(unless (package-installed-p 'json-mode)
  (package-install 'json-mode))

;; Yaml-mode
(unless (package-installed-p 'yaml-mode)
  (package-install 'yaml-mode))

;; Which-key
(unless (package-installed-p 'which-key)
  (package-install 'which-key))

;; Beacon
(unless (package-installed-p 'beacon)
  (package-install 'beacon))

;; Helpful
(unless (package-installed-p 'helpful)
  (package-install 'helpful))

;; Deadgrep
(unless (package-installed-p 'deadgrep)
  (package-install 'deadgrep))

;; Projectile
(unless (package-installed-p 'projectile)
  (package-install 'projectile))

(unless (package-installed-p 'highlight-thing)
  (package-install 'highlight-thing))

;; ob-sql-mode
(unless (package-installed-p 'ob-sql-mode)
  (package-install 'ob-sql-mode))

;; sqlformat
(unless (package-installed-p 'sqlformat)
  (package-install 'sqlformat))

;; python-docstring
(unless (package-installed-p 'python-docstring)
  (package-install 'python-docstring))

;; company
(unless (package-installed-p 'company)
  (package-install 'company))

;; company-anaconda
(unless (package-installed-p 'company-anaconda)
  (package-install 'company-anaconda))

;; flycheck
(unless (package-installed-p 'flycheck)
  (package-install 'flycheck))

;; sphinx-doc
(unless (package-installed-p 'sphinx-doc)
  (package-install 'sphinx-doc))

;; hydra
(unless (package-installed-p 'hydra)
  (package-install 'hydra))

;; py-isort
;; (unless (package-installed-p 'py-isort)
;;   (package-install 'py-isort))

;; org-superstar
(unless (package-installed-p 'org-superstar)
  (package-install 'org-superstar))

;; markdown-mode
(unless (package-installed-p 'markdown-mode)
  (package-install 'markdown-mode))

;; org-roam
(unless (package-installed-p 'org-roam)
  (package-install 'org-roam))

;; doom-modeline
(unless (package-installed-p 'doom-modeline)
  (package-install 'doom-modeline))

;; dashboard
(unless (package-installed-p 'dashboard)
  (package-install 'dashboard))

;; centered-window
(unless (package-installed-p 'centered-window)
  (package-install 'centered-window))

;; Setting a file for installations and one for customization
(setq custom-file (locate-user-emacs-file "coffeemacs.el"))
(load custom-file 'noerror 'nomessage)

;;; init.el ends here
