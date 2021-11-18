;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; DO NOT EDIT THIS FILE DIRECTLY
;; This is a file generated from a literate programing source file located at
;; https://github.com/starr-dusT/dotfiles/blob/master/.doom.d/doomed.org
;; You should make any changes there and regenerate it from Emacs org-mode
;; using org-babel-tangle (C-c C-v t)

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
;; (setq user-full-name "John Doe"
;;      user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
;; (setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
;; (setq display-line-numbers-type t)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(setq this-system "kestrel")
(setq all-systems '("kestrel" "basilisk" "adjudicator"))
(setq only-kestrel '("kestrel"))
(setq only-basilisk '("basilisk"))

(setq user-full-name "Tyler Starr"
      user-mail-address "starrtyler88@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))
;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-gruvbox)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

(map! :leader
      ; Add to the "open" menu in Doom
      (:prefix-map ("o" . "open")
       (:prefix-map ("o" . "org-ql")
       :desc "views" "v" #'org-ql-view
       :desc "Weekly Agenda" "w" (cmd! (org-ql-view "Weekly Agenda"))
       :desc "Tasks to Refile" "r" (cmd! (org-ql-view "Tasks to Refile"))
       :desc "This Weeks Progress" "p" (cmd! (org-ql-view "This Weeks Progress")))))

(setq org-directory "~/documents/org/")
(setq org-capture (directory-files-recursively
                   (concat org-directory "gtd/capture/") "\.org$"))
(setq org-agenda (directory-files-recursively
                  (concat org-directory "gtd/agenda/") "\.org$"))
(setq org-todo (directory-files-recursively
                (concat org-directory "gtd/todo/") "\.org$"))
(setq org-agenda-files (append org-capture org-agenda org-todo))

(setq org-roam-directory (concat org-directory "roam"))
(setq org-roam-db-location (concat org-directory "roam/org-roam.db"))

(after! org
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "CHASE(c)" "WIP(p)" "WAIT(w@/!)"
                    "GAVE(g@/!)" "|" "KILL(k@/!)" "DONE(d)")))
  ; TODO add (1)...(10) numbers for task ordering (replacing "next")
  (setq org-todo-keyword-faces
      (quote (("TODO"  :foreground "red"          :weight bold)
              ("CHASE" :foreground "red"          :weight bold)
              ("WIP"   :foreground "blue"         :weight bold)
              ("NEXT"  :foreground "orange"       :weight bold)
              ("GAVE"  :foreground "orange"       :weight bold)
              ("WAIT"  :foreground "orange"       :weight bold)
              ("KILL"  :foreground "forest green" :weight bold)
              ("DONE"  :foreground "forest green" :weight bold))))

  (setq org-use-tag-inheritance t)
  (setq org-tags-exclude-from-inheritance '("prj" "prg" "subprj"))
  (setq org-tag-alist
    '((:startgroup)
      ; Put mutually exclusive tags here
      (:endgroup)
      ("@home" . ?h)
      ("@work" . ?w)
      ("question" . ?q)
      ("exclude" . ?e)
      ("prj" . ?p)
      ("subprj" . ?s)
      ("prg" . ?P)
      ("habit" . ?h)
      ("me" . ?m)
      ("Aaron" . ?a)
      ("Landon" . ?l)
      ("Valerie" . ?v)
      ("David" . ?d)))

  (setq org-capture-todo (concat org-directory "gtd/capture/inbox.org"))

  (setq org-capture-templates
        (doct '(("todo" :keys "t"
                 :file org-capture-todo
                 :template ("* TODO %?" "%U"))
                ("question" :keys "q"
                 :file org-capture-todo
                 :template ("* TODO Find out %? :question:"
                            "%U"))
                ("habit" :keys "h"
                 :file org-capture-todo
                 :template ("* NEXT %? :habit:exclude:" "%U"
                            "SCHEDULED: %(format-time-string
                                         \"%<<%Y-%m-%d %a .+1d/3d>>\")"
                            ":PROPERTIES:" ":STYLE: habit"
                            ":REPEAT_TO_STATE: NEXT" ":END:"))
                ("meeting" :keys "m"
                 :file org-capture-todo
                 :template ("* NEXT %? :meeting:exclude:"
                            "%U")))))

  (setq org-refile-targets (quote ((nil :maxlevel . 3)
                                   (org-agenda-files :maxlevel . 3))))
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (setq org-agenda-start-day "0d")
  (setq org-agenda-show-future-repeats 'next)
  (setq org-agenda-custom-commands
        '(("w" "Super awesome work agenda"
           ((agenda "" ((org-agenda-overriding-header "")
                        (org-super-agenda-groups
                         '((:name "SCHEDULED"
                                  :time-grid t
                                  :date today
                                  :todo "TODAY"
                                  :scheduled today
                                  :not (:todo "WIP"))
                           (:name "DUE"
                                  :deadline today)
                           (:name "OVERDUE"
                                  :deadline past)
                           (:name "DUE SOON"
                                  :deadline future)
                           (:name "PAST SCHEDULED"
                                  :scheduled past
                                  :not (:todo "WIP"))
                           (:name "SCHEDULED SOON"
                                  :scheduled future
                                  :not (:todo "WIP"))
                           (:discard (:anything t))))))
            (alltodo "" ((org-agenda-overriding-header "")
                         (org-super-agenda-groups
                          '((:name "TASK TO REFILE"
                                   :file-path "gtd/capture")
                            (:name "IN PROGRESS"
                                   :todo "WIP")
                            (:name "IMPORTANT"
                                   :priority "A")
                            (:name "QUESTION"
                                   :tag "question")
                            (:name "STANDALONE TASKS"
                                   :and (:todo "TODO"
                                         :not (:tag "exclude")))
                            (:name "WAITING"
                                   :and (:todo "WAIT"
                                         :not (:tag "exclude")))
                            (:name "DELEGATED"
                                   :and (:todo "GAVE"
                                         :not (:tag "exclude")))
                            (:discard (:anything t))))))))))

  (setq org-ql-weekly-agenda
      (cons "Weekly Agenda"
              (lambda ()
              "Open agenda for week."
              (interactive)
              (org-agenda nil "t"))))

  (setq org-ql-refile-tasks
      (cons "Tasks to Refile"
              (lambda ()
              "Find tasks to refile."
              (interactive)
              (org-ql-search (list org-capture-todo)
              '(or (not (done))
                      (done))
              :title "Tasks to Refile"
              :sort '(date priority todo)
              :super-groups '((:name "Todos"
                              :not (:tag "note")))))))

  (setq org-ql-weeks-progress
      (cons "This Weeks Progress"
          (lambda ()
          "launch an agenda-like view at the specified date."
          (interactive)
          (let* ((ts (ts-now))
                  (beg-of-week (->> ts
                                  (ts-adjust 'day (- (ts-dow (ts-now))))
                                  (ts-apply :hour 0 :minute 0 :second 0)))
                  (end-of-week (->> ts
                                  (ts-adjust 'day (- 6 (ts-dow (ts-now))))
                                  (ts-apply :hour 23 :minute 59 :second 59))))
          (org-ql-search (org-agenda-files)
              '(ts-active :from beg-of-week :to end-of-week)
              :title "Week Overview"
              :sort '(date priority todo)
              :super-groups '((:name "Late"
                              :scheduled past
                              :deadline past)
                              (:name "Today"
                              :time-grid t
                              :scheduled today
                              :deadline today)
                              (:name "Coming Up"
                              :scheduled future
                              :deadline future)))))))

  (defun open-org-ql-project (program)
    (org-ql-search (list program)
      '(and (todo)
            (not (children)))
     :super-groups '((:auto-outline-path t))))
 
  (setq org-ql-project-view
      (cons "Project View"
          (lambda ()
          "launch a project view for a given program."
          (interactive)
          (ivy-read "Project: "
                    (org-agenda-files)
                    :require-match t
                    :action #'open-org-ql-project))))

  (setq org-super-agenda-header-map (make-sparse-keymap))
  (setq org-ql-views
        (list org-ql-weekly-agenda
              org-ql-refile-tasks
              org-ql-weeks-progress
              org-ql-project-view))
  (after! org-agenda
      (org-super-agenda-mode))

(setq org-startup-folded t)
(setq org-src-preserve-indentation t)) ; Close the after! org expression

(add-hook 'haskell-mode-hook #'hindent-mode)
(setq lsp-enable-on-type-formatting nil)

(defun ts/next-fixme()
       "Move cursor and delete next FIXME in beancount file."
       (interactive)
       (re-search-forward "FIXME" nil t)
       (delete-char -5))

(defun ts/next-fixme-replace()
       "Move cursor, delete next FIXME, and insert account in beancount file."
       (interactive)
       (ts/next-fixme)
       (call-interactively 'beancount-insert-account)
       (call-interactively 'beancount-align-numbers))
(beancount-mode)
(define-key beancount-mode-map (kbd "C-c F") #'ts/next-fixme)
(define-key beancount-mode-map (kbd "C-c f") #'ts/next-fixme-replace)
