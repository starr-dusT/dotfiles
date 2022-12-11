from libqtile.config import Match, Rule
from libqtile import hook

dgroups_app_rules = [Rule(Match(wm_type=["confirm",
                                         "download",
                                         "notification",
                                         "toolbar",
                                         "splash",
                                         "dialog",
                                         "error",
                                         "file_progress",
                                         "confirmreset",
                                         "makebranch",
                                         "maketag",
                                         "branchdialog",
                                         "pinentry",
                                         "sshaskpass"]),
                          float=True),
                     Rule(Match(wm_class=["lutris", "league of legends.exe", "leagueclientux.exe"]),
                          float=True,
                          break_on_match=True)]
@hook.subscribe.client_new
def floating_dialogs(window):
    dialog = window.window.get_wm_type() == 'dialog'
    transient = window.window.get_wm_transient_for()
    if dialog or transient:
        window.floating = True
