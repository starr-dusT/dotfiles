{
  config,
  lib,
  user,
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
    role = mkOption {
      type = types.nullOr types.str;
      default = null;
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
      role = "${cfg.role}";
      tokenFile = "/run/agenix/kube/token"; 
      clusterInit = if "${cfg.role}" == "server" then true else false;
      serverAddr = if "${cfg.role}" == "agent" then "https://tetragon:6443" else ""; 
    };
  };
}
