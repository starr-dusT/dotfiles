{ ... }:
{
  flake.modules.nixos.kde-theme =
    { config, ... }:
    let
      user = "${config.preferences.user}";
      hostname = "${config.preferences.hostname}";
    in
    {
      # Set user avatar
      system.activationScripts.userAvatar.text = ''
        install -Dm644 ${../../../../../../resources/img/avatars/${user}.png} \
          /var/lib/AccountsService/icons/${user}
      '';

      # Set default (no user) locksreen wallpaper based on hostname
      system.activationScripts.systemWallpaper.text = ''
        install -Dm644 ${../../../../../../resources/img/wallpapers/${hostname}.png} \
          /var/lib/AccountsService/wallpaper
      '';
      environment.etc."plasmalogin.conf".text = ''
        [Greeter][Wallpaper][org.kde.image][General]
        Image=file:///var/lib/AccountsService/wallpaper
      '';

      home-manager.users.${user} = {
        programs.plasma = {
          kscreenlocker = {
            appearance.wallpaper = "/home/${user}/.local/share/chezmoi/resources/img/wallpapers/${hostname}.png";
          };
          workspace = {
            clickItemTo = "open";
            colorScheme = "BreezeDark";
            lookAndFeel = "org.kde.breezedark.desktop";
            iconTheme = "breeze-dark";
            theme = "breeze-dark";
            wallpaper = "/home/${user}/.local/share/chezmoi/resources/img/wallpapers/${hostname}.png";
          };
        };
      };
    };
}
