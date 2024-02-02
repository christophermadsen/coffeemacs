;;; -*- lexical-binding: t; -*-
;;; -*- coding: utf-8 -*-

;;; Coffeemacs --- Emacs config on Windows by Whiskeybear
;;; Commentary:
;;; This is the file for setting personal info and paths.  Do not try and load
;;; or install packages, that should strictly be done in init.el and
;;; coffeemacs.el
;;; Code:

;; Set name and email.
(setq user-full-name "Your Name Here"
      user-mail-address "")

;; Start dired in a specific folder
(setq default-directory "Path to your home/working directory")

;; Path to Graphviz executable for org-roam visualizer
(setq org-roam-graph-executable "")

;; Path to executable for viewing the org-roam visualization
(setq org-roam-graph-viewer "")

;; Set executable for the find command executable
(setq find-program "")
;;; personal.el ends here
