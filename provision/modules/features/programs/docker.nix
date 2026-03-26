{ ... }:
{
  flake.modules.nixos.docker =
    { config, ... }:
    {
      virtualisation.docker.enable = true;
      users.users.${config.preferences.user} = {
        extraGroups = [ "docker" ];
      };
    };
}
