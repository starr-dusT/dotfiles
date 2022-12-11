;;; ux.el -*- lexical-binding: t; -*-

;; Author: Tyler Starr

;; Commentary

;; Custom configuration of emacs ux elements

;;; File Management

(crafted-package-install-package 'dirvish)
(dirvish-override-dired-mode)

(crafted-package-install-package 'treemacs)

;;; Window Management

(crafted-package-install-package '(burly :host github
                                         :repo "alphapapa/burly.el"
                                         :branch "master"))
(tab-bar-mode)
(burly-tabs-mode)

;;; Keybinds

(crafted-package-install-package 'which-key)
(which-key-mode)


;;; Provide the module
(provide 'ux)
