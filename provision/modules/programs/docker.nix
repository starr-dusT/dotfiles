{ config, lib, pkgs, user, ... }:

let cfg = config.modules.programs.docker;
in {
  options.modules.programs.docker.enable = lib.mkEnableOption "docker";

  config = lib.mkIf cfg.enable {
    virtualisation.containers.enable = true;
    virtualisation = {
      podman = {
        enable = true;
        dockerCompat = true;
        defaultNetwork.settings.dns_enabled = true;
      };
    };
    
    users.users.${user} = {
      extraGroups = [ "podman" ];
    };
  };
}
