;;; +gtd.el -*- lexical-binding: t; -*-

(after! org
  (setq org-capture (directory-files-recursively
                     (concat org-directory "gtd/capture/") "\.org$"))
  (setq org-agenda (directory-files-recursively
                    (concat org-directory "gtd/agenda/") "\.org$"))
  (setq org-todo (directory-files-recursively
                  (concat org-directory "gtd/todo/") "\.org$"))
  (setq org-note (directory-files-recursively
                  (concat org-directory "gtd/note/") "\.org$"))
  (setq org-agenda-files (append org-capture org-agenda org-todo))
  (setq org-default-notes-file org-note)

  (setq org-todo-keywords
        (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|"
                          "CANCELLED(c@/!)"))))

  ; TODO add (1)...(10) numbers for task ordering (replacing "next")
  (setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("CHASE" :foreground "red" :weight bold)
              ("WIP" :foreground "blue" :weight bold)
              ("GAVE" :foreground "orange" :weight bold)
              ("KILL" :foreground "forest green" :weight bold)
              ("DONE" :foreground "forest green" :weight bold))))

(setq org-use-tag-inheritance t)

(setq org-tag-alist
  '((:startgroup)
    ; Put mutually exclusive tags here
    (:endgroup)
    ("@home" . ?H)
    ("@work" . ?W)
    ("note" . ?n)
    ("question" . ?q)
    ("habit" . ?h)
    ("recurring" . ?r)))

  (setq org-capture-todo (concat org-directory "gtd/capture/inbox.org"))
  (setq org-capture-note (concat org-directory "gtd/capture/note.org"))

  (setq org-capture-templates
        (doct '(("personal" :keys "p"
                 :children (("todo" :keys "t"
                             :file org-capture-todo
                             :template ("* TODO %? :@home:" "%U"))
                            ("question" :keys "q"
                             :file org-capture-todo
                             :template ("* TODO Find out %? :question:@home:"
                                        "%U"))
                            ("habit" :keys "h"
                             :file org-capture-todo
                             :template ("* NEXT %? :habit:@home:" "%U"
                                        "SCHEDULED: %(format-time-string
                                         \"%<<%Y-%m-%d %a .+1d/3d>>\")"
                                        ":PROPERTIES:" ":STYLE: habit"
                                        ":REPEAT_TO_STATE: NEXT" ":END:"))
                            ("meeting" :keys "m"
                             :children (("reoccuring" :keys "r"
                                         :file org-capture-todo
                                         :template ("* NEXT %? :meeting:@home:"
                                                    "%U" "SCHEDULED:
                                                     %(format-time-string
                                                       \"%<<%Y-%m-%d %a +7d>>\")"
                                                    ":PROPERTIES:"
                                                    ":REPEAT_TO_STATE: NEXT"
                                                    ":END:"))))
                            ("note" :keys "n"
                             :file org-capture-note
                             :template ("* %? :note:@home:" "%U"))))
                ("work" :keys "w"
                 :children (("todo" :keys "t"
                             :file org-capture-todo
                             :template ("* TODO %? :@work:" "%U"))
                            ("question" :keys "q"
                             :file org-capture-todo
                             :template ("* TODO Find out %? :question:@work:"
                                        "%U"))
                            ("habit" :keys "h"
                             :file org-capture-todo
                             :template ("* NEXT %? :habit:@work:" "%U"
                                        "SCHEDULED: %(format-time-string
                                                      \"%<<%Y-%m-%d %a .+1d/3d>>\")"
                                        ":PROPERTIES:" ":STYLE: habit"
                                        ":REPEAT_TO_STATE: NEXT" ":END:"))
                            ("meeting" :keys "m"
                             :children (("reoccuring" :keys "r"
                                         :file org-capture-todo
                                         :template ("* NEXT %? :meeting:@work:"
                                                    "%U" "SCHEDULED:
                                                     %(format-time-string
                                                     \"%<<%Y-%m-%d %a +7d>>\")"
                                                    ":PROPERTIES:"
                                                    ":REPEAT_TO_STATE: NEXT"
                                                    ":END:"))))
                            ("note" :keys "n"
                             :file org-capture-note
                             :template ("* %? :note:@work:" "%U")))))))

(setq org-refile-targets (quote ((nil :maxlevel . 3)
                                 (org-agenda-files :maxlevel . 3))))

(advice-add 'org-refile :after 'org-save-all-org-buffers)

; Open a view with need buffers for planning!
(defun ts/replace-with-agenda-collection ()
       (interactive)
       (org-agenda nil "t")
       (delete-other-windows)
       (get-buffer "*Org Agenda*")
       (org-ql-view "Week Overview")
       (org-ql-view-sidebar))

(setq org-agenda-start-day "0d")

(setq org-agenda-custom-commands
      '(("t" "Agenda for today" agenda ""
         ((org-agenda-overriding-header "Today's agenda")
          (org-agenda-span 'day)))))

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
            (org-ql-search (list org-capture-todo org-capture-note)
            '(or (not (done))
                    (done))
            :title "Tasks to Refile"
            :sort '(date priority todo)
            :super-groups '((:name "Todos"
                            :not (:tag "note"))
                            (:name "Notes"
                                :tag "note"))))))

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

(setq org-super-agenda-header-map (make-sparse-keymap))

(setq org-ql-views
      (list org-ql-weekly-agenda
            org-ql-refile-tasks
            org-ql-weeks-progress))
(after! org-agenda
(org-super-agenda-mode)))   ; Close the after! org expression from
                            ; Org File Paths
