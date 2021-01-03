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

  (setq org-capture-todo (concat org-directory "gtd/capture/inbox.org"))
  (setq org-capture-note (concat org-directory "gtd/capture/note.org"))

  (setq org-capture-templates
        (doct '(("personal" :keys "p"
                 :children (("todo" :keys "t"
                             :file org-capture-todo
                             :template ("* TODO %? :@home:personal:" "%U"))
                            ("question" :keys "q"
                             :file org-capture-todo
                             :template ("* TODO Find out %? :@home:question:"
                                        "%U"))
                            ("habit" :keys "h"
                             :file org-capture-todo
                             :template ("* NEXT %? :@home:habit:" "%U"
                                        "SCHEDULED: %(format-time-string
                                         \"%<<%Y-%m-%d %a .+1d/3d>>\")"
                                        ":PROPERTIES:" ":STYLE: habit"
                                        ":REPEAT_TO_STATE: NEXT" ":END:"))
                            ("meeting" :keys "m"
                             :children (("reoccuring" :keys "r"
                                         :file org-capture-todo
                                         :template ("* NEXT %? :@home:meeting:"
                                                    "%U" "SCHEDULED:
                                                     %(format-time-string
                                                     \"%<<%Y-%m-%d %a +7d>>\")"
                                                    ":PROPERTIES:"
                                                    ":REPEAT_TO_STATE: NEXT"
                                                    ":END:"))))
                            ("note" :keys "n"
                             :file org-capture-note
                             :template ("* %? :@home:note:" "%U")))))))

  (setq org-super-agenda-header-map (make-sparse-keymap))
  (org-super-agenda-mode)

  (setq org-agenda-custom-commands
        '(("d" "Daily Agenda"
           ((agenda "" ((org-agenda-span 'day)
                        (org-super-agenda-groups
                         '((:name "LATE"
                                  :face (:underline t)
                                  :deadline past)
                           (:name "TODAY"
                                  :time-grid t
                                  :scheduled today
                                  :deadline today)))))
            (todo "" ((org-agenda-overriding-header "")
                         (org-super-agenda-groups
                         '((:name "--- PROJECTS ---"
                                  :children t)
                           (:discard (:anything t))))))))))

  (setq org-tag-alist
    '((:startgroup)
      ; put mutually exclusive tags here
      (:endgroup)
      ("@home" . ?h)
      ("@work" . ?w)
      ("note" . ?n)
      ("question" . ?q)
      ("habit" . ?h)
      ("recurring" . ?r)))

)
