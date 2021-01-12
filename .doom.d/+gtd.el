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

(setq org-super-agenda-header-map (make-sparse-keymap))

(defun ts/org-daily-agenda ()
  "launch an agenda-like view at the specified date."
  (interactive)
  (org-ql-search (org-agenda-files)
    '(and (or (ts-active :on today)
              (deadline auto)
              (scheduled :to today))
          (not (done)))
    :title "Daily Agenda"
    :super-groups '((:name "Home"
                     :tag "@home")
                    (:name "Work"
                     :tag "@work"))))

(after! org-agenda
  (org-super-agenda-mode))) ; Close the after! org expression from
                            ; Org File Paths
