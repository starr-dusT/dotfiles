{ ... }:
{
  flake.modules.nixos.kde-shortcuts =
    { config, ... }:
    let
      user = "${config.preferences.user}";
    in
    {
      home-manager.users.${user} = {
        programs.plasma = {
          shortcuts = {
            kwin = {
              "Switch to Desktop 1" = "Meta+Q";
              "Switch to Desktop 2" = "Meta+W";
              "Switch to Desktop 3" = "Meta+E";
              "Switch to Desktop 4" = "Meta+R";
              "Window to Desktop 1" = "Meta+Shift+Q";
              "Window to Desktop 2" = "Meta+Shift+W";
              "Window to Desktop 3" = "Meta+Shift+E";
              "Window to Desktop 4" = "Meta+Shift+R";
              "Window Close" = "Meta+D";
              "Window Maximize" = "Meta+T";
              "Window Fullscreen" = "Meta+F";
              "Window Above Other Windows" = "Meta+S";

              "Switch to Previous Screen" = "Meta+H";
              "Switch to Next Screen" = "Meta+L";
              "Window to Previous Screen" = "Meta+Shift+H";
              "Window to Next Screen" = "Meta+Shift+L";
              "Window Quick Tile Left" = "Meta+J";
              "Window Quick Tile Right" = "Meta+K";

              "Edit Tiles" = "none"; # Unset default Meta+T
              "Overview" = "none"; # Unset default Meta+W
            };
            ksmserver = {
              "Lock Session" = "Meta+Space";
            };
            "services/chromium-browser.desktop"._launch = "Meta+!";
            "services/com.mitchellh.ghostty.desktop"._launch = "Meta+@";
            "services/org.kde.dolphin.desktop"._launch = "Meta+#";
            "services/discord.desktop"._launch = "Meta+$";
          };
        };
      };
    };
}
