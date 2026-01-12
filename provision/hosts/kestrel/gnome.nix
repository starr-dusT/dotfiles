{ pkgs, user, ... }:
{
  environment.systemPackages = with pkgs; [
    gnomeExtensions.executor # Execute shell commands display output top bar
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
        "right-commands-json" = ''
          {"commands":[
              {
                "isActive": true,
                "command": "echo \"( $(hostname) ) \"",
                "interval": 300,
                "uuid": "732cd6de-ff5f-46a7-b8bb-51d1c621cc60"
              },
              {
                "isActive":true,
                "command":"echo \"( $(subwoofer-volume.sh -r)dB )\"",
                "interval":5,
                "uuid":"732cd6de-ff5f-46a7-b8bb-51d1c621cc62"
              }
            ]
          }'';
      };
    };
  };
}
