{
  config,
  lib,
  user,
  hostname,
  ...
}:

let
  cfg = config.modules.optional.programs.k3s;
in
{
  options.modules.optional.programs.k3s = with lib; {
    enable = lib.mkOption {
      type = types.bool;
      default = false;
    };
  };

  config = lib.mkIf cfg.enable {
    networking.firewall.allowedTCPPorts = [
      6433
      2379
      2380
    ];
    networking.firewall.allowedUDPPorts = [ 8472 ];

    age.secrets."kube/token" = {
      file = ../../../secrets/kube/token.age;
      owner = "${user}";
      group = "users";
    };

    services.k3s = {
      enable = true;
      role = if "${hostname}" == "vortex-1" then "server" else "agent";
      tokenFile = "/run/agenix/kube/token";
      clusterInit = if "${hostname}" == "vortex-1" then true else false;
      serverAddr = if "${hostname}" != "vortex-1" then "https://vortex-1:6443" else "";
    };
  };
}
