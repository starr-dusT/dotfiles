{ ... }:
{
  flake.modules.nixos.kde-panel =
    { config, pkgs, ... }:
    let
      user = "${config.preferences.user}";
    in
    {
      environment.systemPackages = with pkgs; [
        kdotool # xdotool clone for KDE Wayland
        plasma-applet-commandoutput # Run a command periodically and render its output
      ];

      home-manager.users.${user} = {
        programs.plasma = {
          panels = [
            {
              screen = 1;
              location = "top";
              floating = false;
              height = 40;
              widgets = [
                { kickoff = { }; }
                {
                  iconTasks = {
                    launchers = [
                      "applications:chromium-browser.desktop"
                      "applications:com.mitchellh.ghostty.desktop"
                      "applications:org.kde.dolphin.desktop"
                      "applications:discord.desktop"
                    ];
                  };
                }
                "org.kde.plasma.panelspacer"
                {
                  systemTray.items = {
                    shown = [
                      "org.kde.plasma.volume"
                      "org.kde.plasma.networkmanagement"
                    ];
                  };
                }
                {
                  digitalClock = {
                    time.format = "24h";
                  };
                }
                "org.kde.plasma.showdesktop"
                {
                  name = "com.github.zren.commandoutput";
                  config = {
                    command = "kdotool get_desktop";
                    interval = 100;
                    waitForCompletion = true;
                  };
                }
              ];
            }
          ];
        };
      };
    };
}
