{
  config,
  lib,
  user,
  hostname,
  pkgs,
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
      6443
      2379
      2380
      31111
    ];
    networking.firewall.allowedUDPPorts = [ 8472 ];

    environment.systemPackages = with pkgs; [
      k9s
      kubernetes-helm
      fluxcd
      jq
      python3
    ];

    services.openiscsi = {
      enable = true;
      name = "${hostname}-initiatorhost";
    };
    systemd.services.iscsid.serviceConfig = {
      PrivateMounts = "yes";
      BindPaths = "/run/current-system/sw/bin:/bin";
    };

    age.secrets."kube/token" = {
      file = ../../../secrets/kube/token.age;
      owner = "${user}";
      group = "users";
    };

    services.k3s = {
      enable = true;
      role = if (lib.strings.hasInfix "vortex" "${hostname}") then "server" else "agent";
      tokenFile = "/run/agenix/kube/token";
      clusterInit = if "${hostname}" == "vortex-1" then true else false;
      serverAddr = if "${hostname}" == "vortex-1" then "" else "https://vortex-1:6443";
    };
  };
}
