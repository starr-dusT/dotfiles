;;; org.el -*- lexical-binding: t; -*-

;; Author: Tyler Starr

;; Commentary

;; Custom configuration for org-mode stuff!

;;; Org file locations

;; Set default working directory for org files
(setq org-directory "~/documents/org")
;; Set default locations to store notes
(setq org-default-notes-file "~/documents/org/capture/refile.org")
;; Set agenda files
(setq org-agenda-files (quote ("~/documents/org/capture"
                              "~/documents/org/capture/agendas"
                              "~/documents/org/capture/bookmarks"
                              "~/documents/org/capture/notes")))

;;; Set Todo Options

;; Set keywords for todo items
(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" ))))

;; Set colors for todo items
(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("NEXT" :foreground "blue" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("HOLD" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold))))

;; Set tags based on todo changes
(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t))
              ("HOLD" ("WAITING") ("HOLD" . t))
              (done ("WAITING") ("HOLD"))
              ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
              ("NEXT" ("WAITING") ("CANCELLED") ("HOLD")))))
;; open org-capture
(global-set-key (kbd "C-c c") 'org-capture)

(defvar ts-capture-prmt-history nil
  "History of prompt answers for org capture.")

(defun ts/prmt (prompt variable)
  "PROMPT for string, save it to VARIABLE and insert it."
  (make-local-variable variable)
    (set variable (read-string (concat prompt ": ") nil ts-capture-prmt-history)))

;; Capture templates for: TODO tasks, Notes, appointments, and meetings
(setq org-capture-templates
      (quote (("t" "todo" entry (file "~/documents/org/capture/refile.org")
              "* TODO %?\n%U\n%a\n")
              ("r" "respond" entry (file "~/documents/org/capture/refile.org")
              "* TODO Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n")
              ("w" "workout" entry (file+datetree "~/documents/org/tracking/workout.org")
              "* Test: %(ts/prmt \"Hey\" 'lel) - %(ts/prmt \"Hey1\" 'lel)")
              ("n" "note" entry (file "~/documents/org/capture/refile.org")
              "* %? :NOTE:\n%U\n%a\n")
              ("m" "Meeting" entry (file "~/documents/org/capture/refile.org")
              "* MEETING with %? :MEETING:\n%U")
              ("h" "Habit" entry (file "~/documents/org/capture/refile.org")
              "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n"))))

;; Targets include this file and any file contributing to the agenda - up to 9 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                (org-agenda-files :maxlevel . 9))))
;; Use full outline paths for refile targets - we file directly with IDO
(setq org-refile-use-outline-path t)
;; Targets complete directly with IDO
(setq org-outline-path-complete-in-steps nil)
;; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))
;; Exclude DONE state tasks from refile targets
(defun bh/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))
(setq org-refile-target-verify-function 'bh/verify-refile-target)


(crafted-package-install-package 'org-super-agenda)
(org-super-agenda-mode)
(setq org-super-agenda-header-map (make-sparse-keymap))

(setq org-agenda-custom-commands
      '(("c" "Custom Agenda"
        ((agenda "" ((org-agenda-span 'day)
                      (org-super-agenda-groups
                            '((:name "--- LATE ---"
                                :face (:underline t)
                                :deadline past
                                :order 1)
                              (:name "--- DUE TODAY ---"
                                :time-grid t
                                :deadline today
                                :order 2)
                              (:name "--- SCHEDULED TODAY ---"
                                :time-grid t
                                :date today
                                :scheduled today
                                :order 3)
                              (:name ""
                                :discard (:anything)
                                :order 99))
                            )))))))

;; Configure common tags
(setq org-tag-alist
  '((:startgroup)
    ; Put mutually exclusive tags here
    (:endgroup)
    ("@errand" . ?E)
    ("@home" . ?H)
    ("@work" . ?W)
    ("agenda" . ?a)
    ("planning" . ?p)
    ("publish" . ?P)
    ("batch" . ?b)
    ("note" . ?n)
    ("idea" . ?i)
    ("thinking" . ?t)
    ("recurring" . ?r)))

;;; Provide the module
(provide 'custom-org)
