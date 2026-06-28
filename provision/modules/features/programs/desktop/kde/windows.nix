{ ... }:
{
  flake.modules.nixos.kde-windows =
    { config, ... }:
    let
      user = "${config.preferences.user}";
    in
    {
      home-manager.users.${user} = {
        programs.plasma = {
          configFile = {
            kwinrulesrc = {
              General = {
                count = 2;
                rules = "c760e3b6-07b2-443f-a4a1-9b787d98df63,db5c45aa-c941-40b3-887a-4b769008c411";
              };
              c760e3b6-07b2-443f-a4a1-9b787d98df63 = {
                Description = "Discord";
                desktops = "Desktop_1";
                desktopsrule = 3;
                position = "580,120";
                positionrule = 3;
                screen = 1;
                screenrule = 3;
                size = "1400,1200";
                sizerule = 3;
                wmclass = "discord";
                wmclassmatch = 1;
              };
              db5c45aa-c941-40b3-887a-4b769008c411 = {
                Description = "Steam";
                desktops = "Desktop_4";
                desktopsrule = 3;
                maximizehoriz = true;
                maximizehorizrule = 3;
                maximizevert = true;
                maximizevertrule = 3;
                screenrule = 3;
                wmclass = "steam";
                wmclassmatch = 1;
              };
            };
          };
        };
      };
    };
}
