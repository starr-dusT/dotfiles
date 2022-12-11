;;; config.el -*- lexical-binding: t; -*-

;; Author: Tyler Starr

;; Commentary

;; Custom configuration for SystemCrafter's crafted-emacs

;;; Enable crafted modules
(require 'crafted-defaults)    ; Sensible default settings for Emacs
(require 'crafted-updates)     ; Tools to upgrade Crafted Emacs
(require 'crafted-completion)  ; selection framework based on `vertico`
(require 'crafted-ui)          ; Better UI experience (modeline etc.)
(require 'crafted-windows)     ; Window management configuration
(require 'crafted-editing)     ; Whitspace trimming, auto parens etc.
(require 'crafted-evil)        ; An `evil-mode` configuration
(require 'crafted-org)         ; org-appear, clickable hyperlinks etc.
(require 'crafted-project)     ; built-in alternative to projectile
(require 'crafted-speedbar)    ; built-in file-tree

;;; Enable local modules
(require 'theme)      ; Colors and non-interactive UI elements
(require 'ux)         ; Interactive UI elements and window management
(require 'custom-org) ; All the custom bits for org-mode and accesories!

;;; Further settings and customizations follow...
