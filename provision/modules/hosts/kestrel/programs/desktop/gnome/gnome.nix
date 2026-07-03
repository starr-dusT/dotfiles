{ ... }:
{
  flake.modules.nixos.kestrelGnome =
    { pkgs, config, ... }:
    {
      environment.systemPackages = with pkgs; [
        gnomeExtensions.executor # Execute shell commands display output top bar
      ];

      home-manager.users.${config.preferences.user} = {
        xdg.configFile = {
          "autostart/discord.desktop".text = ''
            [Desktop Entry]
            Type=Application
            Name=Discord
            Exec=${pkgs.discord}/bin/discord --start-minimized
            NoDisplay=true
          '';
          "autostart/steam.desktop".text = ''
            [Desktop Entry]
            Type=Application
            Name=Steam
            Exec=${pkgs.steam}/bin/steam -silent
            NoDisplay=true
          '';
          "autostart/mattermost.desktop".text = ''
            [Desktop Entry]
            Type=Application
            Name=Discord
            Exec=${pkgs.mattermost-desktop}/bin/mattermost-desktop
            NoDisplay=true
          '';
        };

        dconf.settings = {
          "org/gnome/shell" = {
            enabled-extensions = [
              "executor@raujonas.github.io"
            ];
          };
          "org/gnome/shell/extensions/executor" = {
            "click-on-output-active" = false;
            "center-active" = false;
            "left-active" = false;
            "right-active" = true;
            "right-commands-json" = ''
              {"commands":[
                  {
                    "isActive": true,
                    "command": "echo \" $(hostname) \"",
                    "interval": 300,
                    "uuid": "732cd6de-ff5f-46a7-b8bb-51d1c621cc60"
                  },
                  {
                    "isActive":true,
                    "command":"echo \" $(subwoofer-volume.sh -r)\"",
                    "interval":5,
                    "uuid":"732cd6de-ff5f-46a7-b8bb-51d1c621cc62"
                  }
                ]
              }'';
          };
        };
      };
    };
}
