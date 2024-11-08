{ config, lib, pkgs, user, home-manager, ... }:
{
  environment.systemPackages = with pkgs; [
    gnomeExtensions.executor # Execute shell commands display output top bar.
  ];

  home-manager.users.${user} = {
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
        "right-commands-json" = ''{"commands":[{"isActive":true,"command":"pySVS 54:B7:E5:57:1A:7B --volume=A | grep -oP \"(?<=VOLUME': )-?\\d+\" | xargs -I{} echo \"󰓃 {}dB\"","interval":20,"uuid":"732cd6de-ff5f-46a7-b8bb-51d1c621cc60"}]}'';
      };
    };
  };
}
