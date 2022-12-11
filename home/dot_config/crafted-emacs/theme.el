;;; theme.el -*- lexical-binding: t; -*-

;; Author: Tyler Starr

;; Commentary

;; Custom theming for emacs

;;; Colors

;; Provide nice premade themes
(crafted-package-install-package 'doom-themes)

;; Disable default theme and enable gruvbox
(disable-theme 'deeper-blue)
(load-theme 'doom-gruvbox t)

;;; Provide the module
(provide 'theme)
