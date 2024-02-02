;;; -*- lexical-binding: t; -*-
;;; -*- coding: utf-8 -*-

;;; Coffeemacs --- Emacs config on Windows by Whiskeybear
;;; Commentary:
;;; This is the main file for the customization of coffeemacs.
;;; Keymaps are at the bottom of the file.
;;; Code:

;; Automatically create missing parents when making a new file
(defun my-create-non-existent-directory ()
      (let ((parent-directory (file-name-directory buffer-file-name)))
        (when (and (not (file-exists-p parent-directory))
                   (y-or-n-p (format "Directory `%s' does not exist! Create it?" parent-directory)))
          (make-directory parent-directory t))))
(add-to-list 'find-file-not-found-functions #'my-create-non-existent-directory)

;;;; GENERAL
(menu-bar-mode -1) ;; Disables the menu bar
(scroll-bar-mode -1) ;; Disables the scrollbar
(tool-bar-mode -1) ;; Disables the toolbar
(show-paren-mode 1) ;; highlight matching parentheses
(fset 'yes-or-no-p 'y-or-n-p) ;; y-n for yes-no
(global-anzu-mode +1) ;; Anzu - tracking search numbers (C-s)
(global-display-line-numbers-mode +1) ;; Show line numbers in all modes
(electric-pair-mode +1) ;; Pair parentheses
(global-visual-line-mode t) ;; Wraps lines to fit a window

;; Scrolling
(setq mouse-wheel-scroll-amount '(0.03)) ;; 3 lines at a time
(setq mouse-wheel-progressive-speed nil) ;; Don't accelerate scrolling
(setq mouse-wheel-follow-mouse t) ;; Scroll window under mouse
(setq-default pixel-scroll-precision-mode t) ;; Smooth scrolling over images
(setq ring-bell-function 'ignore)

;; Frame Title
(setq frame-title-format
      '("emacs: " (:eval (if (buffer-file-name)
                             (abbreviate-file-name (buffer-file-name)) "%b"))))

;; Startup window size
(set-frame-height (selected-frame) 70)
(set-frame-width (selected-frame) 200)

;; Defaults
(setq-default
 message-log-max 1000
 cursor-type 'bar ;; Make cursor a bar
 fill-column 79 ;; Break line at 79 chars, use M-q
 standard-indent 4 ;; How many spaces to use for a tab
 tab-always-indent 'complete ;; Will either complete or indent depending on the context
 indent-tabs-mode nil ;; Prevents Emacs from replacing multiple spaces with tabs
 indicate-empty-lines t ;; Shows empty lines
 use-dialog-box nil ;; Prevents Emacs UI dialog pop ups
 column-number-mode t ;; Columns numbers

 ;; Some Emacs files version control
 delete-old-versions t ;; Deletes old versions
 kept-new-versions 6
 kept-old-versions 2
 version-control t
 vc-make-backup-files t
 backup-directory-alist '((".*" . "~/.emacs.d/emacs-backups")) ;; Assuming .emacs.d placement

 ;; Calendar settings
 european-calendar-style t
 calendar-date-style 'european
 calendar-week-start-day 1

 ;; Time and date display settings
 display-time-24hr-format t
 display-time-day-and-date t
 display-time-string-forms
 '((if (and (not display-time-format) display-time-day-and-date)
       (format-time-string "%a %b %e " now) "")
   (format-time-string (or display-time-format
                           (if display-time-24hr-format "%H:%M" "%-I:%M%p"))
                       now))
 calendar-time-display-form
 '(24-hours ":" minutes
            (if time-zone " (") time-zone (if time-zone ")"))

 ;; Some ibuffer formatting
 ibuffer-use-other-window t
 ibuffer-formats
 '((mark modified read-only locked
         " " (name 36 36 :left :elide)
         " " (size 9 -1 :right)
         " " (mode 16 16 :left :elide) " " filename-and-process)
   (mark " " (name 16 -1) " " filename))
 ) ;; end of setq-default

;;;; STRAIGHT SETUP
;; See https://sqrtminusone.xyz/posts/2021-05-01-org-python/
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
;;;; END - STRAIGHT SETUP

;;;; ORG-MODE SETUP
;;;; fix org roam bug as of 8/8/23
;;;; https://github.com/org-roam/org-roam/issues/2361
;;;; The easiest fix was to specify a slightly older version of org in the
;;;; straight 'lockfile' at straight/versions/default.el

;; fix for org-roam bug
;; (setq straight-lockfile-dir "~/.emacs.d/straight/versions/")
;; (if (not (file-directory-p straight-lockfile-dir))
;;     (make-directory straight-lockfile-dir))
;; (copy-file ~/.emacs.d/bugfix.el (concat straight-lockfile-dir "default.el"))

(straight-use-package 'org)
(use-package org
  :demand t
  :custom (org-export-backends '(ascii html icalendar odt md org))
  :hook (org-mode . turn-on-auto-fill)
  :config
  (setq org-todo-keywords
        '((sequence "ONGOING(o!)" "TODO(t!)" "VERIFY(v@)" "REVIEW(r!)" "HOLD/WAIT(h@)" "WISHLIST(w!)"
                    "|" "DONE(d)" "DELEGATED" "DROPPED"))
        org-time-stamp-custom-formats '("<%a %d %B>" . "<%d %b %a %H:%M>")
        org-export-allow-bind-keywords t
        org-src-preserve-indentation t
        org-src-window-setup 'current-window
        org-log-done 'time
        org-log-into-drawer t
        org-confirm-babel-evaluate nil
        ;; Use relative links for org-insert-link
        org-link-file-path-type 'relative
        ;; Indent org file according to heading levels
        org-startup-indented t
        ;; Allow for inline display of remote images
        org-display-remote-inline-images 'cache
        ;; Display images when starting up an org file including them
        org-startup-with-inline-images t)
  
  ;; Refresh inline images after executing a block
  (add-hook 'org-babel-after-execute-hook
            (lambda () (org-display-inline-images nil t)))
  
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (sql . t)
     (python . t)
     (jupyter . t)
     )))

;; (use-package org
;;   :demand t
;;   :custom (org-export-backends '(ascii html icalendar odt md org))
;;   :hook (org-mode . turn-on-auto-fill)
;;   :config
;;   (setq org-todo-keywords
;;         '((sequence "ONGOING(o!)" "TODO(t!)" "VERIFY(v@)" "REVIEW(r!)" "HOLD/WAIT(h@)" "WISHLIST(w!)"
;;                     "|" "DONE(d)" "DELEGATED" "DROPPED"))
;;         org-time-stamp-custom-formats '("<%a %d %B>" . "<%d %b %a %H:%M>")
;;         org-export-allow-bind-keywords t
;;         org-src-preserve-indentation t
;;         org-src-window-setup 'current-window
;;         org-log-done 'time
;;         org-log-into-drawer t
;;         org-confirm-babel-evaluate nil
;;         ;; Use relative links for org-insert-link
;;         org-link-file-path-type 'relative
;;         ;; Indent org file according to heading levels
;;         org-startup-indented t
;;         ;; Allow for inline display of remote images
;;         org-display-remote-inline-images 'cache
;;         ;; Display images when starting up an org file including them
;;         org-startup-with-inline-images t)
  
;;   ;; Refresh inline images after executing a block
;;   (add-hook 'org-babel-after-execute-hook
;;             (lambda () (org-display-inline-images nil t)))
  
;;   (org-babel-do-load-languages
;;    'org-babel-load-languages
;;    '((emacs-lisp . t)
;;      (sql . t)
;;      (python . t)
;;      (jupyter . t)
;;      )))

;; Remove confirmation for running org src block
(setq org-confirm-babel-evaluate nil)

;; Get theme color for org babel src blocks
(require 'color)
(set-face-attribute 'org-block nil :background
                    (color-darken-name
                     (face-attribute 'default :background) 3))

 ;; Fix org block indentation
(setq-default org-src-preserve-indentation t)

;; This stops org from making inline images their actual widths.
;; Use "#+ATTR_ORG: :width some-number" instead, at the top of an image link.
(setq org-image-actual-width nil)

;; Change the org-ellipsis symbol for collapsed headers to something else
(setq org-ellipsis "↴")

;; Unset minimize
(global-unset-key (kbd "C-z"))
;;;; END - ORG-MODE SETUP

;; Font
;; (set-face-font 'default "-unknown-DejaVu Sans Mono-normal-normal-normal-*-13-*-*-*-m-0-iso10646-1")
(set-face-font 'default "-unknown-DejaVuSansM Nerd Font-normal-normal-normal-mono-13-*-*-*-p-*-iso10646-1")

;; UTF-8 Encoding
;; https://rufflewind.com/2014-07-20/pasting-unicode-in-emacs-on-windows
(set-selection-coding-system 'utf-16-le) ;; Windows API is built on UTF-16
(set-language-environment "UTF-8")
(setq uniquify-buffer-name-style 'forward
      locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

;; Visual headers (utf-8 bullets) for org-mode
(use-package org-superstar
  :ensure t)
(require 'org-superstar)
(add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))

(with-eval-after-load 'org-superstar
  (set-face-attribute 'org-superstar-item nil :height 1.2)
  (set-face-attribute 'org-superstar-header-bullet nil :height 1.2)
  (set-face-attribute 'org-superstar-leading nil :height 1.3))
;; Set different bullets, with one getting a terminal fallback.
(setq org-superstar-headline-bullets-list
  '( "◙" "●" "◕" "◑" "◔" "◎" "○" "◌")) ;; Circles
  ;; '("⭓" "⭔" "◆" "◇" "▶" "▷")) ;; Geometric shapes
  ;; '("▢" "⚀" "⚁" "⚂" "⚃" "⚄" "⚅" "▣")) ;; Dice
  ;; '("◉" "◈" "▣" "○" "◇" "▢" "▷")) ;; Geometric shapes 2
  ;; '("✺" "✹" "✸" "✷" "✶" "✦" "✧")) ;; Stars
  ;; '("✽" "✾" "✿" "❀" "❃" "❉")) ;; Flowers
  ;; '("※" "⁜" "⁂" "⁘" "⁙" "▷")) ;; Symbols and asterisks
  ;; '("⭓" "⭔" "⭗" "⭖" "⬭" "⯍" "⯏")) ;; Misc.

;; Stop cycling bullets to emphasize hierarchy of headlines.
(setq org-superstar-cycle-headline-bullets nil)

;; Hide away leading stars on terminal.
(setq org-superstar-leading-fallback ?\s)

;; For help and functionality inside org jupyter-python src blocks
(use-package hydra)

;; Mode for viewing json files
(use-package json-mode)

;; Mode for viewing yaml files
(use-package yaml-mode
  :mode "\\.dvc\\'"
  :mode "/Dvcfile\\'")

;; Shows your keybindings following what you've currently entered
(use-package which-key
  :config (which-key-mode))

;; Highlights your cursor when you jump around a file
(use-package beacon
  :config (beacon-mode 1))

;; An extension to the default help (C-h) in Emacs
(use-package helpful
  :bind (("C-h j" . helpful-at-point)
         ("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-c C-d" . helpful-at-point)
         ("C-h F" . helpful-function)))

;; Alternative fast way to search with R.I.P grep
(use-package deadgrep
  :commands deadgrep
  :bind ("C-c s" . deadgrep))

;; Gives some useful commands / keybindings
(use-package crux :ensure t)


;; Highlights the same instances of a symbol/word. Some words are filtered.
(use-package highlight-thing
  :demand t
  :hook ((prog-mode . highlight-thing-mode)
         (org-mode . highlight-thing-mode))
  :config
  (setq highlight-thing-exclude-thing-under-point t
        highlight-thing-case-sensitive-p t
        highlight-thing-ignore-list '("False" "True", "return", "None", "if", "else", "self", "import", "from", "in", "def", "class")))

;; Project management, makes it super easy to find files in a project
(use-package projectile
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))
;;;; END - GENERAL

;;;; DASHBOARD
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  :diminish
  (dashboard-mode page-break-lines-mode)
  :custom
  (dashboard-center-content t)
  (dashboard-startup-banner 4)
  (dashboard-items '((recents . 10)))
  (dashboard-icon-type 'nerd-icons)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  :custom-face
  (dashboard-heading ((t (:foreground "#f1fa8c" :weight bold))))
  :hook
  (after-init . dashboard-setup-startup-hook))

(setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
(setq dashboard-banner-logo-title "Aah, you're back. Drink your coffee while it's warm . . .")
(setq dashboard-startup-banner "~/.emacs.d/coffee-isometric-dashboard.png")

 ;; Use `nerd-icons' package, we're using this for DOOM modeline too
;; (setq dashboard-icon-type 'nerd-icons)
;; (setq dashboard-set-heading-icons t)
;; (setq dashboard-set-file-icons t)
;; (setq dashboard-heading-icons nil) 
;;;; END - DASHBOARD

;;;; THEMES
(use-package doom-themes)
(use-package solarized-theme)

(defun light ()
  "Turn on light theme."
  (interactive)
  (load-theme 'doom-solarized-light t))

(defun dark ()
  "Turn on dark theme."
  (interactive)
  (load-theme 'doom-material-dark t))

(dark)
;;;; END - THEMES
    
;;;; AUTO-COMPLETE IN THE MINI-BUFFER (E.G. FILE FINDER) - VERTICO
(use-package vertico
  :init
  (vertico-mode))

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  ;; )

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))
;;;; END - AUTO-COMPLETE IN THE MINI-BUFFER (E.G. FILE FINDER) - VERTICO

;; Windows PowerShell in Emacs
(use-package powershell
  :straight t)

;;;; SQL FORMATTING
(use-package sqlformat
  :ensure t
  :config (setq sqlformat-command 'pgformatter
                sqlformat-args '("-w79" "-s2"))
  :init (add-hook 'sql-mode-hook #'sqlformat-on-save-mode))

;; SQL in org code blocks
(use-package ob-sql-mode)

;; Use pgformatter by default
(setq sqlformat-command 'pgformatter)
;;;; END - SQL FORMATTING

;; ;;;; PYTHON SPECIFICS
;; Jupyter
(use-package jupyter :straight t)

;; ;; This is the basic emacs python IDE, worth a try if all else doesn't work.
;; (use-package elpy
;;   :ensure t
;;   :init
;;   (elpy-enable))

;; Using isort for clean-up commands (Make sure to pip install isort)
(use-package py-isort-windows
    :hook (before-save . py-isort-before-save)
    :config
    (setq py-isort-options '("-l=79" "-m=3" "--tc" "--ca")))

;; ;; Remove unused or add missing python imports
(use-package pyimport)

;; Black python formatting (Make sure to pip install black)
(use-package python-black
  :demand t
  :after python
  :config (setq python-black-extra-args '("-l" "79"))
  :hook (python-mode . python-black-on-save-mode))

;; Inside org src blocks we need to use python-black-region instead
;; Requires pip install black-macchiato for that to work
(defun format-src-block ()
  "Run black on an org source block."
  (interactive)
  ;; save-excursion is somehow overwritten, hence move point back manually
  ;;(save-excursion
  (let ((p (point)))
    (org-babel-mark-block)
    (let ((lang (nth 0 (org-babel-get-src-block-info))))
      (when (string-equal lang "python")
        (python-black-partial-dwim)
        (py-isort-region))
      (when (string-equal lang "sql")
        (call-interactively 'sqlformat)))
    (deactivate-mark)
    (goto-char p)))

;; Toggle black formatting when saving for python code
(defun toggle-black-on-save ()
  (interactive)
  (if (member 'python-black-on-save-mode python-mode-hook)
      (progn
        (message "Black on save.")
        (remove-hook 'python-mode-hook 'python-black-on-save-mode))
    (message "No black on save.")
    (add-hook 'python-mode-hook 'python-black-on-save-mode)))

(use-package python-docstring
  :hook (python-mode . python-docstring-mode))

;; Auto-completion, only hooks to programming modes, minibuffer uses Vertico.
(use-package company
  :demand t
  :commands company-indent-or-complete-common
  :hook ((prog-mode . company-mode)
         (inferior-python-mode . company-mode)
         (org-mode . company-mode))
  :init
  (setq
  company-backends '(company-capf company-files)
  ;; From DOOM
  ;; Only search the current buffer for `company-dabbrev' (a backend that
  ;; suggests text your open buffers). This prevents Company from causing
  ;; lag once you have a lot of buffers open.
  company-dabbrev-other-buffers nil

  ;; Make `company-dabbrev' fully case-sensitive, to improve UX with
  ;; domain-specific words with particular casing.
  company-dabbrev-ignore-case nil
  company-dabbrev-downcase nil))

;; Configure some settings for python mode (notice the use of ipython)
(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :config
  (setq-default python-indent-offset 4)
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "--simple-prompt --classic"
        ;; guess indents, but don't show message in minibuffer
        python-indent-guess-indent-offset t
        python-indent-guess-indent-offset-verbose nil
        ;; One less newline at end of docstrings
        python-fill-docstring-style 'pep-257-nn)
  ;; Some faster navigation, these keys are otherwise unbound
  :bind (:map python-mode-map
              ("M-n" . 'python-nav-forward-defun)
              ("M-p" . 'python-nav-backward-defun)
              ("C-M-n" . 'python-nav-forward-statement)
              ("C-M-p" . 'python-nav-backward-statement)
              ("C-c f" . 'python-black-buffer)))

;; We can use this for Company
(use-package anaconda-mode
  :hook ((python-mode . anaconda-mode)
         (python-mode . anaconda-eldoc-mode)
         ;; Seems to work in jupyter repl, but not inferior python
         (jupyter-repl-mode . anaconda-eldoc-mode)
         (jupyter-org-interaction-mode . anaconda-eldoc-mode)))

;; Using an anaconda backend for the Company autocompletion
(use-package company-anaconda
  :demand t
  :after (python company)
  :config
  (add-to-list 'company-backends 'company-anaconda))

;; Syntax Checking - You may have to go to "Manage App Execution Aliases" in
;; Windows and turn it off for the "App Installer python.exe" and "python3.exe"
(use-package flycheck
  :demand t
  :config
  (add-to-list 'flycheck-disabled-checkers 'python-pylint) ;; Remove pylint and use flake8
  (add-hook 'prog-mode-hook #'flycheck-mode))

;; User flake8, this can be removed if an appropriate system python has Nflake8 installed
;; (setq flycheck-python-flake8-executable "~/AppData/Local/Programs/Python/Python312/Lib/site-packages/flake8")

;; Automatically insert sphinx docstring stubs
(use-package sphinx-doc
  :hook (python-mode . sphinx-doc-mode)
  :config
  (setq sphinx-doc-exclude-rtype t))

;; Remove indent guessing warning
(setq python-indent-guess-indent-offset-verbose nil)
;;;; END - PYTHON SPECIFICS

;;;; OVERRIDE BASE PYTHON WITH JUPYTER KERNELS
(defvar jupyter-python-override t "Flag to override python src blocks to jupyter.")

(use-package jupyter
  :demand t
  :after (:all org python)
  :config
  (setq org-babel-default-header-args:jupyter-python
        '((:session . "py")
          (:kernel . "python3")
          (:async . "yes")))
  (org-babel-jupyter-override-src-block "python") ;; Get normal python src blocks to use jupyter-python
  )

;; Refreshes kernels when switching envs
(defun my/jupyter-refresh-kernelspecs ()
  "Refresh Jupyter kernelspecs."
  (interactive)
  (jupyter-available-kernelspecs t))
;;;; END - OVERRIDE BASE PYTHON WITH JUPYTER KERNELS

;;;; IMPROVE DIRED
;; Adds some neat functionalities to the dired
(load "dired-x")
(add-hook 'dired-load-hook
          (lambda ()
            (setq dired-dotfiles-show-p t
                  dired-listing-switches "-alh"
                  dired-omit-files (concat
                                    dired-omit-files
                                    "\|"
                                    (rx (or (: bos (or "\.DS_Store"
                                                       "__MACOSX"
                                                       "ltximg"
                                                       ".git"
                                                       ".dvc")
                                               eos)
                                            (: bos "__pycache__"
                                               "_minted-"
                                               "\.~lock\."
                                               "!.*pdf$")
                                            ".ipynb_checkpoints" "\.~.*#")))
                  dired-omit-extensions
                  (append dired-omit-extensions
                          '("orgx" "pyc" "pyo" "lprof" "bak")))))
;; Somehow this didn't apply, at least for dired inside TRAMP
(setq dired-listing-switches "-alh")
(add-hook 'dired-after-readin-hook 'dired-omit-mode)
;;;; END - IMPROVE DIRED


;;;; ORG SKELETONS
;; For date insertion into templates
(require 'calendar)

;; Job hunting
(define-skeleton post-job-application-doc
  "In-buffer settings info for a emacs-org file."
  "Skeleton for documented a job application"
  "Location: \n"
  "Type: \n"
  "Department: \n"
  "Position: \n"
  "Requirements: \n"
  "\n"
  "Why did I apply?: \n"
  )

;; Generic experiment report template
(define-skeleton report-template
  "Template for experiment reports."
  ""
  "#+TITLE: "(skeleton-read "Title: ") \n
  "#+AUTHOR: " (insert user-full-name) \n
  "#+EMAIL: "  (insert user-mail-address) \n
  "#+DATE: " (insert (calendar-date-string (calendar-current-date) nil)) \n
  "#+STARTUP: showall" \n
  "#+PROPERTY: header-args :exports both "
  ":session " (skeleton-read "Session Name: ")
  " :kernel " (skeleton-read "Jupyter Kernel Name (optional): ")
  " :cache no" \n
  ":PROPERTIES:
#+OPTIONS: ^:nil
:END:" \n \n
  "* TODO Introduction

* Imports and Environment Variables
:PROPERTIES:
:visibility: folded
:END:

#+name: imports
#+begin_src python :results silent

#+end_src

#+name: env
#+begin_src python :results silent

#+end_src

* TODO Conclusion")

(defun setup-report-refresh-buffer ()
    "Revert buffer without confirmation.
Used to ensure session parameters are usable immediately."
    (interactive)
    (report-template)
    (save-buffer)
    (revert-buffer :ignore-auto :noconfirm))
;;;; END - ORG SKELETONS

;;;; LOAD CUSTOM LISP
(add-to-list 'load-path "~/.emacs.d/lisp/")
(require 'org-block-extra)
(require 'miscellaneous)
(require 'py-isort-windows)

;; ox-ipynb is an extension for exporting .org to .ipynb files.
(require 'ox-ipynb)

;; The export of .org to .ipynb with ox-ipynb won't work with regular python
;; source blocks in org, and we use this because we override the base python
;; with jupyter kernels, but this github issue:
;; https://github.com/jkitchin/ox-ipynb/issues/13 provides a work around.
;; It says it might delete output blocks, but it hasn't been an issue so far.
(push '(python . (kernelspec . ((display_name . "Python 3")
                                (language . "python")
                                (name . "python3"))))
      ox-ipynb-kernelspecs)

(push '(python . (language_info . ((codemirror_mode . ((name . ipython)
                                                      (version . 3)))
                                   (file_extension . ".py")
                                   (mimetype . "text/x-python")
                                   (name . "python")
                                   (nbconvert_exporter . "python")
                                   (pygments_lexer . "ipython3")
                                   (version . "3.5.2"))))
      ox-ipynb-language-infos)

;; Even smoother scrolling over images
(require 'iscroll)

;;;; END - LOAD CUSTOM LISP

;;;; MARKDOWN
(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
              ("C-c C-e" . markdown-do)))
;;;; END - MARKDOWN

;;;; ORG-ROAM
(setq org-notes-dir "~/fooly-cooly/")
(if (not (file-directory-p org-notes-dir))
    (make-directory org-notes-dir))

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename org-notes-dir))
  :bind (("C-c r l" . org-roam-buffer-toggle)
         ("C-c r f" . org-roam-node-find)
         ("C-c r g" . org-roam-graph)
         ("C-c r i" . org-roam-node-insert)
         ("C-c r a" . org-roam-alias-add)
         ("C-c r c" . org-roam-capture)
         ("C-c r t" . org-roam-tag-add)
         ;; Dailies
         ("C-c r j" . org-roam-dailies-capture-today))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode 1)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

(add-to-list 'display-buffer-alist
             '("\\*org-roam\\*"
               (display-buffer-in-direction)
               (direction . right)
               (window-width . 0.33)
               (window-height . fit-window-to-buffer)))

;; ;; Setup for writing daily logs
;; (setq org-daily-log-dir "~/daily-logs/")
;; (if (not (file-directory-p org-daily-log-dir))
;;     (make-directory org-daily-log-dir))
;; (setq org-roam-dailies-directory org-daily-log-dir)
;; (setq org-roam-dailies-capture-templates
;;       '(("d" "default" entry
;;          "* Previous Workday\n- %?\n\n* Today\n+ "
;;          :target (file+head "%<%A-%Y-%m-%d>.org"
;;                             "#+title: %<Daily Log %A-%Y-%m-%d>\n"))))
;; END - ORG-ROAM

;; Makes it possible to center buffer content in the window
(use-package centered-window :ensure t)

;; Some small functions for using a pseudo-presentation mode
(defun toggle-presentation-mode ()
  "Toggle on/off presentation mode."
  (interactive)
  (if (get 'toggle-pm 'state)
      (progn
        (presentation-mode-off)
        (put 'toggle-pm 'state nil))
    (progn
      (presentation-mode-on)
      (put 'toggle-pm 'state t))))

(defun presentation-mode-on ()
  "Toggle on presentation mode."
  (interactive)
  (org-toggle-emphasis)
  (global-display-line-numbers-mode -1)
  (visual-line-mode -1)
  (centered-window-mode t))

(defun presentation-mode-off ()
  "Toggle off presentation mode."
  (interactive)
  (org-toggle-emphasis)
  (global-display-line-numbers-mode t)
  (visual-line-mode t)
  (centered-window-mode -1))

;; Add a transparency toggle-transparency for .80 alpha
(defun toggle-transparency ()
  "Toggle transparent windows on/off."
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (if (eq
     (if (numberp alpha)
         alpha
       (cdr alpha)) ; may also be nil
     100)
    (set-frame-parameter nil 'alpha '(95 . 0))
    (set-frame-parameter nil 'alpha '(100 . 100)))))

;; 'arrayify' function for quickly making lists of unmarked things
(defun arrayify (start end quote)
  "Turn strings on newlines into a QUOTEd, comma-separated one-liner."
  (interactive "r\nMQuote: ")
  (let ((insertion
         (mapconcat
          (lambda (x) (format "%s%s%s" quote x quote))
          (split-string (buffer-substring start end)) ", ")))
    (delete-region start end)
    (insert insertion)))

;; DOOM MODELINE, a better looking mode line
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-buffer-file-name-style (quote truncate-with-project))
  (doom-modeline-env-python-executable "python3")
  :config
  ;; Add pythonic support to modeline
  (doom-modeline-def-env python
    :hooks   'python-mode-hook
    :command (lambda ()
               (cond ((and (fboundp 'pipenv-project-p)
                           (pipenv-project-p))
                      (list "pipenv" "run"
                            (or doom-modeline-env-python-executable
                                python-shell-interpreter
                                "python")
                            "--version"))
                     ((executable-find "pyenv") (list "pyenv" "version-name"))
                     ((not (null python-shell-virtualenv-root))
                      (list "echo"
                            (format
                             "(%s)"
                             (car (last (split-string python-shell-virtualenv-root "/" 't))))))
                     ((list (or doom-modeline-env-python-executable
                                python-shell-interpreter
                                "python")
                            "--version"))))
    :parser  (lambda (line) (let ((version (split-string line)))
                              (if (length> version 1)
                                  (cadr version)
                                (car version))))))
;;;; END - DOOM MODELINE

;;;; KEYMAPS
;; General
(global-set-key "\C-x\C-b" 'buffer-menu) ;; replace list-buffers with buffer-menu
(global-set-key (kbd "C-c b") 'previous-buffer)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-a") 'beginning-of-indent-or-line)
(global-set-key (kbd "C-c C-w") 'toggle-presentation-mode)

;; Org-mode
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "M-n") 'org-babel-next-src-block)
(global-set-key (kbd "M-p") 'org-babel-previous-src-block)
(org-defkey org-mode-map (kbd "M-P") 'org-insert-block-above)
(org-defkey org-mode-map (kbd "M-N") 'org-insert-block-below)
(org-defkey org-mode-map (kbd "M-D") 'org-kill-block)
(org-defkey org-mode-map (kbd "M-W") 'org-copy-block)
(org-defkey org-mode-map (kbd "C-c c") 'org-execute-block-in-shell)
(org-defkey org-mode-map (kbd "C-c e") 'org-execute-line-in-shell)
(org-defkey org-mode-map (kbd "C-c C-x t") 'org-babel-toggle-results-below-point)
(define-key org-mode-map (kbd "C-c m") 'org-toggle-emphasis)
(org-defkey org-mode-map (kbd "C-c C-x j") 'org-show-just-me)
(org-defkey org-mode-map (kbd "C-c C-x n") 'org-show-next)
(org-defkey org-mode-map (kbd "C-c f") 'format-src-block)
(org-defkey org-mode-map (kbd "C-x :") 'setup-report-refresh-buffer)
(org-defkey jupyter-org-interaction-mode-map (kbd "M-q")
            'python-docstring-fill)

;; Org-roam
(global-set-key (kbd "C-c r I") 'org-id-get-create)

;;;; END - KEYMAPS

;;; coffeemacs.el ends here
