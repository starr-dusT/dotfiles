{ inputs, ... }:
{
  flake.modules.nixos.home-manager =
    { config, ... }:
    let
      user = "${config.preferences.user}";
    in
    {
      imports = [
        inputs.home-manager.nixosModules.home-manager
      ];

      home-manager.users.${user} = {
        home.username = "${user}";
        home.homeDirectory = "/home/${user}";
        programs.home-manager.enable = true;
        home.stateVersion = "23.11";
      };
    };
}
