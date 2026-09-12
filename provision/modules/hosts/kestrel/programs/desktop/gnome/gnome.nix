{ ... }:
{
  flake.modules.nixos.kestrelGnome =
    { pkgs, config, ... }:
    {
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
      };
    };
}
