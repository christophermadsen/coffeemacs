;;; -*- lexical-binding: t; -*-
;;; -*- coding: utf-8 -*-

;;; Coffeemacs --- Emacs config on Windows by Whiskeybear
;;; Commentary:
;;; This is the file for setting personal info and paths.  Do not try and load
;;; or install packages, that should strictly be done in init.el and
;;; coffeemacs.el
;;; Code:

;; Set name and email.
(setq user-full-name "Christopher Buch Madsen"
      user-mail-address "")

;; Start dired in a specific folder
(setq default-directory "C:/Users/Chris/")

;; Set executable for the find command executable
(setq find-program "C:/cygwin64/bin/find.exe")

;; Path to Graphviz executable for org-roam visualizer
(setq org-roam-graph-executable "C:/Graphviz/bin/dot.exe")

;; Path to executable for viewing the org-roam visualization
(setq org-roam-graph-viewer "C:/Program Files/Google/Chrome/Application/chrome.exe")
;;; personal.el ends here
